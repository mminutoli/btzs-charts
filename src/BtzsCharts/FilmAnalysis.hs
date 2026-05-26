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
