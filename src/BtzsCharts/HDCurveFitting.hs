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
  fitCurves
  ) where

import BtzsCharts.Types
import Data.Vector.Storable as VS
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
    hdCurveLabel :: !String,                  -- ^ Experiment Lable
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
-- * @(label, materialDensities)@: The control variable and the associated
--     densities read by the reflection or transmission densitometer on
--     the tested material.
fitHDCurve :: DensityReadings -> (Float, DensityReadings) -> HDCurve
fitHDCurve stepWedgeDensities (label, materialDensities) = HDCurve (show label) (VS.fromList xs) ys
  where
    x0 = VS.minimum stepWedgeDensities
    xs = [x0, x0 + 0.01 .. (VS.maximum stepWedgeDensities)]
    ys = VS.fromList [evaluateV Linear stepWedgeDensities materialDensities x | x <- xs, x < VS.maximum stepWedgeDensities]

-- | Build HDCurve for a series of material experiments.
--
-- Arguments:
-- * @stepWedge@: The step-wedge used for our material test.
-- * @materialTest@: The results of the material test.
fitCurves :: StepTablet -> MaterialTest -> [HDCurve]
fitCurves stepWedge materialTest = Prelude.map (fitHDCurve stepWedgeDensities) experiments
  where
    experiments = (M.toList . results) materialTest
    stepWedgeDensities = densities stepWedge
