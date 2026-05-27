{-|
Module      : BtzsCharts.FilmAnalysis
Description : Analysis of photographic film characteristic curves.
Copyright   : (c) Marco Minutoli, 2025

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX
-}

module BtzsCharts.FilmAnalysis (
    avgGradient,
    idMinTarget,
    findIDmin,
    findIDmax,
    computeNvalue,
    computeNvalues
  ) where

import Control.Monad.Reader
import BtzsCharts.Types
import BtzsCharts.HDCurveFitting

-- | Compute the average gradient of a curve.
--
-- We compute the average gradient of the curve between IDmin and
-- IDmax.
--
-- Arguments:
-- * @curve@: The HD-Curve data fit from the sensitometric measurements.
avgGradient :: HDCurve -> ProcessConfM Double
avgGradient curve = do
  (xMin, yMin) <- findIDmin curve
  flareFactor <- asks flareCompensationFactor
  si <- asks scaleIndex
  let targetDensity = yMin + si * flareFactor
  let (xMax, yMax) = findPointAtDensity targetDensity curve
  return ((yMax - yMin) / (xMax - xMin))

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
findIDmin :: HDCurve -> ProcessConfM (Double, Density)
findIDmin curve = do
  targetOffset <- idMinTarget
  return (findPointAboveFog targetOffset curve)

-- | Find the coordinates of the IDmax on the HD-Curve.
--
-- IDmax is either found on the HD-Curve or projected using the average
-- gradient.
--
-- Arguments:
-- * @curve@: The HD-Curve data fit from the sensitometric measurements.
findIDmax :: HDCurve -> ProcessConfM (Double, Density)
findIDmax curve = do
  (xMin, yMin) <- findIDmin curve
  flareFactor <- asks flareCompensationFactor
  si <- asks scaleIndex
  let targetDensity = yMin + si * flareFactor
  let (x, y) = findPointAtDensity targetDensity curve
  -- We consider the target reached if the density is within a small epsilon
  -- of what the model could actually produce (Dmax).
  let [_, dMax, _, _] = modelParameters curve
  if targetDensity < (dMax - 1e-3)
    then return (x, y)
    else do
      -- If the curve doesn't reach the target density, we project linearly
      -- using the average gradient starting from IDmin.
      gradient <- avgGradient curve
      let deltaLogE = (targetDensity - yMin) / gradient
      let xMax = xMin + deltaLogE
      -- Ensure we always return an exposure greater than the speed point
      return (max (xMin + 0.3) xMax, targetDensity)

-- | Compute the N-value of a HD-curve according to our process.
--
-- Arguments:
-- * @curve@: The HD-Curve data fit from the sensitometric measurements.
computeNvalue :: HDCurve -> ProcessConfM Double
computeNvalue curve = do
  (_, yMin) <- findIDmin curve
  (_, yMax) <- findIDmax curve
  range <- asks zoneRange
  let densityRange = yMax - yMin
      n = (densityRange / 7 * range) - range

  return n

-- | Compute the N-values according to our process and the
--   results of our material test.
--
-- Arguments:
-- * a list of hdCurves.
computeNvalues :: [HDCurve] -> ProcessConfM [Double]
computeNvalues = Prelude.mapM computeNvalue
