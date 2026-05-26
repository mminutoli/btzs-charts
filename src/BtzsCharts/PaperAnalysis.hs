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

-- | Calculate the Paper Speed Point.
-- Returns the target Density (Base+Fog + paperSpeedPointDensity) and the required Log Exposure.
paperSpeedPoint :: HDCurve -> ProcessConfM (Density, Double)
paperSpeedPoint hd = do
  density <- asks paperSpeedPointDensity
  let bpf = basePlusFog hd
      speedDensity = bpf + density
      exposureAtSpeedPoint = exposureForDensity hd speedDensity
  return (speedDensity, exposureAtSpeedPoint)

-- | Calculate the Paper IDmax.
-- Returns the target Density (Base+Fog + (Dmax - Base+Fog) * paperIdMaxPercentage) and the required Log Exposure.
paperIdMax :: HDCurve -> ProcessConfM (Density, Double)
paperIdMax hd = do
  maxDensityPercentage <- asks paperIdMaxPercentage
  let [_, dmax, _, _] = modelParameters hd
      bpf = basePlusFog hd
      idMax = bpf + (dmax - bpf) * maxDensityPercentage
      exposureAtIdMax = exposureForDensity hd idMax
  return (idMax, exposureAtIdMax)

-- | Calculate the Log Exposure Range (LER) or Exposure Scale (ES).
-- LER = Log Exposure at IDmax - Log Exposure at Speed Point.
logExposureRange :: HDCurve -> ProcessConfM Double
logExposureRange hd = do
  (_, exposureAtSpeedPoint) <- paperSpeedPoint hd
  (_, exposureAtIdMax) <- paperIdMax hd
  return (exposureAtIdMax - exposureAtSpeedPoint)

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
  (densityAtSpeedPoint, _) <- paperSpeedPoint hd
  (densityAtIdMax, _) <- paperIdMax hd
  return (densityAtIdMax - densityAtSpeedPoint)
