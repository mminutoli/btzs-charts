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
  computeNvalues
  ) where

import Control.Monad.Reader
import BtzsCharts.Types
import Data.Vector.Storable as VS
import Data.Maybe
import Numeric.GSL.Interpolation (evaluateV, InterpolationMethod(Linear))

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
    outputDensity :: !DensityReadings         -- ^ Density registered on the material
  }
  deriving stock (Show)

-- | Fit a HDCurve on the points read from the development experiments.
--
-- This function fit a curve using a cubic spline on the points obtained through
-- the development tests.
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
  HDCurve devtime (VS.fromList xs) ys
  where
    x0 = VS.minimum stepWedgeDensities
    xs = [x0, x0 + 0.01 .. (VS.maximum stepWedgeDensities)]
    ys = VS.fromList [evaluateV Linear stepWedgeDensities materialDensities x
                     | x <- xs, x < VS.maximum stepWedgeDensities]

-- | Build HDCurve for a series of material experiments.
--
-- Arguments:
-- * @stepWedge@: The step-wedge used for our material test.
-- * @materialTest@: The results of the material test.
fitHDCurves :: StepTablet -> MaterialTest -> [HDCurve]
fitHDCurves stepWedge materialTest =
  Prelude.map (fitHDCurve stepWedgeDensities) experiments
  where
    experiments = (M.toList . results) materialTest
    stepWedgeDensities = densities stepWedge

-- | The value of based plus fog as read from the sensitometer.
--
-- Arguments:
-- * @curve@: The HD-Curve data fit from the sensitometric measurements.
basePlusFog :: HDCurve -> Density
basePlusFog = VS.last . outputDensity

-- | Find the first point on the HD-Curve having at least the target density.
--
-- The function returns the first point on the HD-Curve with at least target
-- density above `basePlusFog` or the highest recorded density otherwise.
-- While not perfect, this solution avoids dealing with Maybe everywhere and
-- fits well the analysis that we need to perform on the curves. Nevertheless
-- if anyone has better ideas, I am open to suggestions.
--
-- Arguments:
-- * @target@: The target above base+fog to determine the speed point.
-- * @curve@: The HD-Curve data fit from the sensitometric measurements.
findPoint :: Density -> HDCurve -> (Int, Density, Density)
findPoint target curve = (pos, e, d)
  where
    targetDensity = basePlusFog curve + target
    idx = VS.findIndexR (>= targetDensity) (outputDensity curve)
    pos = fromMaybe 0 idx
    e = relativeLogExposure curve VS.! pos
    d = outputDensity curve VS.! pos

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
