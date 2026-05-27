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
    estimateCurve
  ) where

import BtzsCharts.HDCurveFitting (HDCurve)

-- | Estimates an HDCurve at a target development time by interpolating 
--   the parameters of the closest measured curves.
estimateCurve :: [HDCurve] -> Float -> HDCurve
estimateCurve _ _ = undefined
