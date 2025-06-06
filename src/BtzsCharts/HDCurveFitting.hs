{-# LANGUAGE DerivingStrategies #-}
-- |
-- Module      : BtzsCharts.HDCurveFitting
-- Description : Fit curves from the data collected in sensitometric experiments.
-- Copyright   : (c) Marco Minutoli, 2025
--
-- License     : BSD-3
-- Maintainer  : Marco Minutoli <mminutoli@gmail.com>
-- Stability   : experimental
-- Portability : POSIX

module BtzsCharts.HDCurveFitting (
  HDCurve,
  fitHDCurve
  ) where

import BtzsCharts.Types (DensityReadings)
import Data.Vector.Storable as VS
import Numeric.GSL.Interpolation (evaluateV, InterpolationMethod(CSpline))
import qualified Control.Category as Curves
import qualified Control.Category as experiments

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
-- * @materialDensities@: The densities read by the reflection or
--     transmission densitometer on the tested material.
fitHDCurve :: DensityReadings -> DensityReadings -> HDCurve
fitHDCurve stepWedgeDensities materialDensities = HDCurve xs ys
  where
    xs = VS.fromList [0.0, 0.01 .. (VS.maximum stepWedgeDensities)]
    ys = VS.fromList [evaluateV CSpline stepWedgeDensities materialDensities x | x <- VS.toList xs]

-- | Build HDCurve for a series of material experiments.
--
-- Arguments:
-- * @stepWedgeDensities@: The densities of the step-wedge used for the
--     experiment.
-- * a list of DensityReadings collected through a series of experiments.
fitCurves :: DensityReadings -> [DensityReadings] -> [HDCurve]
fitCurves stepWedgeDensities = map (fitHDCurve stepWedgeDensities)
