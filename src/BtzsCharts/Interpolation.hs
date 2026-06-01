{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : BtzsCharts.Interpolation
Description : Interpolate parameters of characteristic curves.
Copyright   : (c) Marco Minutoli, 2025

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX
-}
module BtzsCharts.Interpolation (
    estimateCurve,
    timeForGradient
  ) where

import BtzsCharts.Types
import BtzsCharts.HDCurveFitting (HDCurve(..), logisticModel)
import BtzsCharts.FilmAnalysis (avgGradient)
import Data.Vector.Storable as VS
import Data.List (sortOn)

-- | Estimates an HDCurve at a target development time by interpolating
--   the parameters of the closest measured curves.
estimateCurve :: [HDCurve] -> Float -> HDCurve
estimateCurve filmCurves targetTime =
  let sortedCurves = sortOn developmentTime filmCurves
      (before, after) = Prelude.span (\c -> developmentTime c < targetTime) sortedCurves

      -- Handle cases where time matches exactly or is outside bounds
      (curveA, curveB) = case (before, after) of
        ([], (a:_)) -> (a, a)
        ((_:_), []) -> (Prelude.last before, Prelude.last before)
        ((_:_), (a:_)) -> (Prelude.last before, a)
        ([], []) -> error "estimateCurve: no curves provided"

      devTimeA = developmentTime curveA
      devTimeB = developmentTime curveB

      factor = if devTimeA == devTimeB then 0
               else (targetTime - devTimeA) / (devTimeB - devTimeA)

      pA = modelParameters curveA
      pB = modelParameters curveB
      modelParams = Prelude.zipWith (\v1 v2 -> v1 + (v2 - v1) * realToFrac factor) pA pB

      xs = relativeLogExposure curveA
      ys = VS.fromList $ Prelude.concatMap (logisticModel modelParams) (VS.toList xs)
  in HDCurve targetTime xs ys modelParams

-- | Find the development time required to achieve a target average gradient.
--   This involves calculating the gradients of measured curves and interpolating.
timeForGradient :: Double -> [HDCurve] -> Double -> ProcessConfM Float
timeForGradient si filmCurves targetGamma = do
  gammas <- Prelude.mapM (avgGradient si) filmCurves
  let sortedWithG = sortOn (developmentTime . Prelude.fst) (Prelude.zip filmCurves gammas)
      (before, after) = Prelude.span (\(_, g) -> g < targetGamma) sortedWithG

      ((curveA, gA), (curveB, gB)) = case (before, after) of
        ([], (a:_)) -> (a, a)
        ((_:_), []) -> (Prelude.last before, Prelude.last before)
        (_:_, (a:_)) -> (Prelude.last before, a)
        ([], []) -> error "timeForGradient: no curves provided"

      timeDelta = if gA == gB then 0
                  else (targetGamma - gA) / (gB - gA)

      tA = developmentTime curveA
      tB = developmentTime curveB
      estimatedTime = tA + (tB - tA) * realToFrac timeDelta

  return estimatedTime
