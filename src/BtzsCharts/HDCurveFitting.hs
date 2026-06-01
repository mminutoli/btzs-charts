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
  findPointAboveFog,
  findPointAtDensity,
  logisticModel,
  exposureForDensity
  ) where

import BtzsCharts.Types
import Data.Vector.Storable as VS
import Numeric.GSL.Fitting (fitModel)

import qualified Data.Map as M
import qualified Data.Text as T

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
      d = 1 + e
      d2 = d * d
      num = dMax - dMin
      d_dMin = 1 - 1/d
      d_dMax = 1/d
      d_slope = num * (x - infl) * e / d2
      d_infl = -num * slope * e / d2
  in [[d_dMin, d_dMax, d_slope, d_infl]]
logisticDeriv _ _ = error "logisticDeriv: expected 4 parameters"

-- | Fit a single characteristic curve using GSL non-linear least squares.
fitHDCurve :: DensityReadings -> (Float, DensityReadings) -> HDCurve
fitHDCurve xs_raw (devTime, ys_raw) =
  HDCurve devTime xs_raw ys_raw bestParams
  where
    xs_list = VS.toList xs_raw
    ys_list = VS.toList ys_raw
    dat = Prelude.zip xs_list (Prelude.map (:[]) ys_list)
    dMinGuess = VS.head ys_raw
    dMaxGuess = VS.last ys_raw
    slopeGuess = 1.0
    inflGuess = 1.5
    initialGuess = [dMinGuess, dMaxGuess, slopeGuess, inflGuess]
    -- fitModel epsAbs epsRel maxIter (model, deriv) data initialGuess
    (bestParams, _) = fitModel 1e-8 1e-8 1000 (logisticModel, logisticDeriv) dat initialGuess

    -- Generate a smooth curve, potentially extrapolating slightly
    x0 = Prelude.minimum xs_list
    x1 = max 3.0 (Prelude.maximum xs_list)
    xs = [x0, x0 + 0.01 .. x1]
    ys = Prelude.concatMap (logisticModel bestParams) xs

-- | Build HDCurve for a series of material experiments.
--
-- Arguments:
-- * @stepWedge@: The step-wedge used for our material test.
-- * @materialTest@: The results of the material test.
fitHDCurves :: StepTablet -> MaterialTest -> [HDCurve]
fitHDCurves stepWedge (FilmTest d) =
  case validateMeasurements stepWedge (FilmTest d) of
    Left err -> error $ "Validation failed: " Prelude.++ err
    Right () -> Prelude.map (fitHDCurve stepWedgeDensities) (M.toList (filmMeasurements d))
  where
    stepWedgeDensities = densities stepWedge
fitHDCurves stepWedge (PaperTest d) =
  case validateMeasurements stepWedge (PaperTest d) of
    Left err -> error $ "Validation failed: " Prelude.++ err
    Right () -> Prelude.map (\(gradeText, readings) ->
      let gradeVal = case T.unpack gradeText of
                       "00" -> -0.5
                       "0"  -> 0.0
                       s    -> case reads s of
                                 [(val, "")] -> val
                                 _ -> 2.0
      in fitHDCurve stepWedgeDensities (gradeVal, readings)) (M.toList (paperMeasurements d))
  where
    stepWedgeDensities = densities stepWedge

-- | The value of based plus fog as read from the sensitometer.
--
-- Arguments:
-- * @curve@: The HD-Curve data fit from the sensitometric measurements.
basePlusFog :: HDCurve -> Density
basePlusFog curve = case modelParameters curve of
  (dMin:_) -> dMin
  [] -> error "basePlusFog: no parameters found"

-- | Compute the Relative Log Exposure required to reach a specific density.
-- This uses the inverse of the 4-parameter logistic model.
exposureForDensity :: HDCurve -> Density -> Double
exposureForDensity curve targetD =
  case modelParameters curve of
    [dMin, dMax, slope, infl] ->
      let d = max (dMin + 1e-6) (min (dMax - 1e-6) targetD)
          ratio = (dMax - dMin) / (d - dMin)
      in if ratio <= 1.0
         then infl -- Should not happen if d < dMax
         else infl - (1 / slope) * log (ratio - 1)
    _ -> error "exposureForDensity: expected 4 parameters"

-- | Find the point on the HD-Curve corresponding to an absolute density.
findPointAtDensity :: Density -> HDCurve -> (Double, Density)
findPointAtDensity targetDensity curve = (e, targetDensity)
  where
    e = exposureForDensity curve targetDensity

-- | Find the point on the HD-Curve having a density relative to base+fog.
--
-- This function now uses the fitted mathematical model for higher precision.
--
-- Arguments:
-- * @targetOffset@: The target density ABOVE base+fog.
-- * @curve@: The HD-Curve data fit from the sensitometric measurements.
findPointAboveFog :: Density -> HDCurve -> (Double, Density)
findPointAboveFog targetOffset curve = findPointAtDensity targetDensity curve
  where
    targetDensity = basePlusFog curve + targetOffset
