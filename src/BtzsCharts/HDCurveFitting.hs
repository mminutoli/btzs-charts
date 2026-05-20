{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : BtzsCharts.HDCurveFitting
Description : Fit curves from the data collected in sensitometric experiments.
Copyright   : (c) Marco Minutoli, 2025

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX
-}

module BtzsCharts.HDCurveFitting (
  HDCurve(..),
  fitHDCurves,
  basePlusFog,
  findPoint,
  avgGradient,
  idMinTarget,
  findIDmin,
  findIDmax,
  computeNvalues,
  logisticModel,
  exposureForDensity
  ) where

import Control.Monad.Reader
import BtzsCharts.Types
import Data.Vector.Storable as VS
import Numeric.GSL.Fitting (fitModel)

import qualified Data.Map as M

-- | Hurter-Driffield Curves.
--
-- The Hurter-Driffield curves or HD curves or simply characteristic curves
-- represent graphically the response of photographic materials to light (or
-- radiation).
--
-- This libraries uses HDCurve to compute all the information and the supporting
-- plots to use the BTZS system as described in "Beyond the Zone System" by Phil
-- Davis.
data HDCurve =
  HDCurve
  {
    developmentTime:: !Float,                 -- ^ Development Time used.
    relativeLogExposure :: !DensityReadings,  -- ^ Relative Log Exposure
    outputDensity :: !DensityReadings,        -- ^ Density registered on the material
    modelParameters :: ![Double]              -- ^ Parameters of the fitted logistic model
  }
  deriving stock (Show)

-- | The 4-parameter logistic model used for fitting.
-- f(x) = dMin + (dMax - dMin) / (1 + exp(-slope * (x - infl)))
-- fitModel expects: [Double] -> x -> [Double]
logisticModel :: [Double] -> Double -> [Double]
logisticModel [dMin, dMax, slope, infl] x =
  [ dMin + (dMax - dMin) / (1 + exp (-slope * (x - infl))) ]
logisticModel _ _ = error "logisticModel: expected 4 parameters"

-- | The Jacobian of the 4-parameter logistic model.
-- fitModel expects: [Double] -> x -> [[Double]]
logisticDeriv :: [Double] -> Double -> [[Double]]
logisticDeriv [dMin, dMax, slope, infl] x =
  let e = exp (-slope * (x - infl))
      denom = 1 + e
      f_max = 1 / denom
      f_min = 1 - f_max
      df_ds = (dMax - dMin) * e * (x - infl) / (denom * denom)
      df_di = (dMax - dMin) * (-slope) * e / (denom * denom)
  in [[f_min, f_max, df_ds, df_di]]
logisticDeriv _ _ = error "logisticDeriv: expected 4 parameters"

-- | Fit a HDCurve on the points read from the development experiments.
--
-- This function fits a 4-parameter logistic model using non-linear least squares.
--
-- The function expects that the relative order of the DensityReadings is
-- consistent between the two arguments and that the two vectors are of the same
-- length.
--
-- Arguments:
--
-- * @stepWedgeDensities@: The densities of the step-wedge used for the
--     experiment.
-- * @(devtime, materialDensities)@: The control variable and the associated
--     densities read by the reflection or transmission densitometer on
--     the tested material.
fitHDCurve :: DensityReadings -> (Float, DensityReadings) -> HDCurve
fitHDCurve stepWedgeDensities (devtime, materialDensities) =
  HDCurve devtime (VS.fromList xs) (VS.fromList ys) bestParams
  where
    xs_raw = VS.toList stepWedgeDensities
    ys_raw = VS.toList materialDensities
    dat = zip xs_raw (Prelude.map (:[]) ys_raw)

    -- Initial guesses for the 4PL model
    dMin_guess = Prelude.minimum ys_raw
    dMax_guess = Prelude.maximum ys_raw + 0.5
    slope_guess = 2.0
    infl_guess = (Prelude.maximum xs_raw + Prelude.minimum xs_raw) / 2.0
    initialGuess = [dMin_guess, dMax_guess, slope_guess, infl_guess]

    -- Perform the non-linear least squares fit
    -- fitModel epsAbs epsRel maxIter (model, deriv) data initialGuess
    (bestParams, _) = fitModel 1e-8 1e-8 1000 (logisticModel, logisticDeriv) dat initialGuess

    -- Generate a smooth curve, potentially extrapolating slightly
    x0 = Prelude.minimum xs_raw
    x1 = max 3.0 (Prelude.maximum xs_raw)
    xs = [x0, x0 + 0.01 .. x1]
    ys = Prelude.concatMap (logisticModel bestParams) xs

-- | Build HDCurve for a series of material experiments.
--
-- Arguments:
-- * @stepWedge@: The step-wedge used for our material test.
-- * @materialTest@: The results of the material test.
fitHDCurves :: StepTablet -> MaterialTest -> [HDCurve]
fitHDCurves stepWedge test@(FilmTest _ _ _ meas) =
  case validateMeasurements stepWedge test of
    Left err -> error $ "Validation failed: " Prelude.++ err
    Right () -> Prelude.map (fitHDCurve stepWedgeDensities) (M.toList meas)
  where
    stepWedgeDensities = densities stepWedge
fitHDCurves _ (PaperTest{}) = error "Paper analysis not yet implemented"

-- | The value of based plus fog as read from the sensitometer.
--
-- Arguments:
-- * @curve@: The HD-Curve data fit from the sensitometric measurements.
basePlusFog :: HDCurve -> Density
basePlusFog curve = Prelude.head (modelParameters curve)

-- | Compute the Relative Log Exposure required to reach a specific density.
-- This uses the inverse of the 4-parameter logistic model.
exposureForDensity :: HDCurve -> Density -> Double
exposureForDensity curve targetD =
  let [dMin, dMax, slope, infl] = modelParameters curve
      -- Clamp the density slightly to avoid log of zero/negative at asymptotes
      d = max (dMin + 1e-6) (min (dMax - 1e-6) targetD)
      ratio = (dMax - dMin) / (d - dMin)
  in if ratio <= 1.0 
     then infl -- Should not happen if d < dMax
     else infl - (1 / slope) * log (ratio - 1)

-- | Find the first point on the HD-Curve having at least the target density.
--
-- This function now uses the fitted mathematical model for higher precision.
--
-- Arguments:
-- * @target@: The target above base+fog to determine the speed point.
-- * @curve@: The HD-Curve data fit from the sensitometric measurements.
findPoint :: Density -> HDCurve -> (Int, Density, Density)
findPoint target curve = (-1, e, targetDensity)
  where
    targetDensity = basePlusFog curve + target
    e = exposureForDensity curve targetDensity

-- | Compute the average gradient of a curve.
--
-- We compute the average gradient of the curve between IDmin and
-- IDmax or the highest density recorded. This is not necessarily correct as
-- the curve might start to shoulder.  However, for what I have seen so far,
-- modern films tend to have relatively straight characteristic curve. Hence,
-- the error in computing the gradient should be relatively small.
--
-- Arguments:
-- * @curve@: The HD-Curve data fit from the sensitometric measurements.
avgGradient :: HDCurve -> ProcessConfM Double
avgGradient curve = do
  (_, x, y) <- findIDmin curve
  flareFactor <- asks flareCompensationFactor
  si <- asks scaleIndex
  let target = y + si * flareFactor
  let (_, x', y') = findPoint target curve
  return ((y' - y) / (x' - x))

-- | The IDmin target according to the process configuration.
idMinTarget :: ProcessConfM Density
idMinTarget = do
  g <- asks standardAvgGradient
  f <- asks speedPointFactor
  flareFactor <- asks flareCompensationFactor
  return ((g / f) * flareFactor)

-- | Find the coordinates of the IDmin on the HD-Curve.
--
-- Arguments:
-- * @curve@: The HD-Curve data fit from the sensitometric measurements.
findIDmin :: HDCurve -> ProcessConfM (Int, Density, Density)
findIDmin curve = do
  target <- idMinTarget
  return (findPoint target curve)

-- | Find the coordinates of the IDmax on the HD-Curve.
--
-- IDmax is either found on the HD-Curve or projected using the average
-- gradient.
--
-- Arguments:
-- * @curve@: The HD-Curve data fit from the sensitometric measurements.
findIDmax :: HDCurve -> ProcessConfM (Int, Density, Density)
findIDmax curve = do
  (_, _, d) <- findIDmin curve
  flareFactor <- asks flareCompensationFactor
  si <- asks scaleIndex
  let target = d + si * flareFactor
  let (p, x, y) = findPoint target curve
  if y == target
    then return (p, x, y)
    else do
      gradient <- avgGradient curve
      return (-1, x/gradient, target)

-- | Compute the N-value of a HD-curve according to our process.
--
-- Arguments:
-- * @curve@: The HD-Curve data fit from the sensitometric measurements.
computeNvalue :: HDCurve -> ProcessConfM Double
computeNvalue curve = do
  (_, _, y_min) <- findIDmin curve
  (_, _, y_max) <- findIDmax curve
  range <- asks zoneRange
  let density_range = y_min - y_max
      n = (density_range/7 * range) - range

  return n

-- | Compute the N-values according to our process and the
--   results of our material test.
--
-- Arguments:
-- * a list of hdCurves.
computeNvalues :: [HDCurve] -> ProcessConfM [Double]
computeNvalues = Prelude.mapM computeNvalue
