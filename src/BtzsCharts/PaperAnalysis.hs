{-|
Module      : BtzsCharts.PaperAnalysis
Description : Analysis of photographic paper characteristic curves.
Copyright   : (c) Marco Minutoli, 2025

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX
-}

module BtzsCharts.PaperAnalysis (
    paperSpeedPoint,
    paperIdMax,
    logExposureRange,
    isoRange,
    paperDynamicRange
  ) where

import Control.Monad.Reader

import BtzsCharts.Types
import BtzsCharts.HDCurveFitting

-- | Calculate the Paper Speed Point (Target Highlight).
-- Target highlight density = Dmin + y_hl * (Dmax - Dmin)
-- Returns the target Density and the required Log Exposure.
paperSpeedPoint :: HDCurve -> ProcessConfM (Density, Double)
paperSpeedPoint hd = do
  cfg <- ask
  case modelParameters hd of
    [dMin, dMax, _, _] -> do
      let yHl = resolveHlFraction cfg dMin dMax
          targetD = dMin + yHl * (dMax - dMin)
          exposureAtHl = exposureForDensity hd targetD
      return (targetD, exposureAtHl)
    _ -> error "paperSpeedPoint: expected 4 parameters"

-- | Calculate the Paper IDmax (Target Shadow).
-- Target shadow density = Dmin + y_sh * (Dmax - Dmin)
-- Returns the target Density and the required Log Exposure.
paperIdMax :: HDCurve -> ProcessConfM (Density, Double)
paperIdMax hd = do
  cfg <- ask
  case modelParameters hd of
    [dMin, dMax, _, _] -> do
      let ySh = resolveShFraction cfg
          targetD = dMin + ySh * (dMax - dMin)
          exposureAtSh = exposureForDensity hd targetD
      return (targetD, exposureAtSh)
    _ -> error "paperIdMax: expected 4 parameters"

-- | Calculate the Log Exposure Range (LER) or Exposure Scale (ES).
-- LER = Log Exposure at IDmax - Log Exposure at Speed Point = x(y_sh) - x(y_hl).
logExposureRange :: HDCurve -> ProcessConfM Double
logExposureRange hd = do
  (_, exposureAtHl) <- paperSpeedPoint hd
  (_, exposureAtSh) <- paperIdMax hd
  return (exposureAtSh - exposureAtHl)

-- | Calculate the ISO Range.
-- ISO Range = Round(LER * 100 / 10) * 10
isoRange :: HDCurve -> ProcessConfM Int
isoRange hd = do
  logExp <- logExposureRange hd
  let range = 10 * round (100 * logExp / 10)
  return range

-- | Calculate the Dynamic Range of the paper.
-- Dynamic Range = Density at IDmax - Density at Base+Fog.
paperDynamicRange :: HDCurve -> ProcessConfM Density
paperDynamicRange hd = do
  (densityAtHl, _) <- paperSpeedPoint hd
  (densityAtSh, _) <- paperIdMax hd
  return (densityAtSh - densityAtHl)
