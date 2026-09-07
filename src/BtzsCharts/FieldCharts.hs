{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : BtzsCharts.FieldCharts
Description : Implementation of BTZS Field Charts and analytical fitting models.
Copyright   : (c) Marco Minutoli, 2025

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX
-}
module BtzsCharts.FieldCharts (
    FilmCurveStats(..),
    FieldChartModel(..),
    filmSpeedPointExposure,
    calculateCurveStats,
    fitFieldChartModel,
    evaluateTimeModel,
    evaluateSpeedModel
  ) where

import BtzsCharts.Types
import BtzsCharts.HDCurveFitting (HDCurve(..), basePlusFog, exposureForDensity)
import BtzsCharts.FilmAnalysis (avgGradient)
import BtzsCharts.Interpolation (estimateCurve, timeForGradient)

import GHC.Generics (Generic)
import Control.Monad.Reader

-- | Sensitometric statistics computed for a single measured film curve.
data FilmCurveStats =
  FilmCurveStats
  {
    statsDevTime :: !Float,          -- ^ Development time in minutes.
    statsGradient :: !Double,        -- ^ Calculated average gradient (G-bar).
    statsSpeedPoint :: !Double,        -- ^ Log exposure at the film speed point.
    statsIsoSpeed :: !Double,        -- ^ Calculated effective ISO speed.
    statsNValue :: !Double           -- ^ Calculated N-value for the given SBR and LER.
  }
  deriving stock (Generic, Show, Eq)

-- | Analytical polynomial models fit to the measured film curve statistics.
data FieldChartModel =
  FieldChartModel
  {
    timeModelParams :: ![Double],    -- ^ Coefficients for development time model T(N).
    speedModelParams :: ![Double]    -- ^ Coefficients for ISO speed model S(N).
  }
  deriving stock (Generic, Show, Eq)

-- | Find the log exposure at the film speed point (base+fog + filmSpeedPointDensity).
filmSpeedPointExposure :: HDCurve -> ProcessConfM Double
filmSpeedPointExposure curve = do
  speedDensity <- asks filmSpeedPointDensity
  return (exposureForDensity curve (basePlusFog curve + speedDensity))

-- | Compute curve statistics (G-bar, speed point exposure, ISO speed, and N-value)
--   for each measured film curve given a rated ISO speed, printing paper LER, and measuring range in stops.
calculateCurveStats :: [HDCurve] -> Double -> Double -> Double -> ProcessConfM [FilmCurveStats]
calculateCurveStats filmCurves ratedIso ler mr = do
  -- 1. ISO Reference Calibration: Locate reference curve where G-bar = standardAvgGradient (typically 0.58)
  gRefTarget <- asks standardAvgGradient
  tRef <- timeForGradient ler filmCurves gRefTarget
  let refCurve = estimateCurve filmCurves tRef
  eRef <- filmSpeedPointExposure refCurve
  normalSbr <- asks effectiveNormalSbr

  -- 2. Compute stats for each measured curve in the family
  let computeStats curve = do
         let t = developmentTime curve
         g <- avgGradient ler curve
         e_speed <- filmSpeedPointExposure curve

         -- EFS = ratedIso * 2^((E_ref - e_speed) / 0.3)
         let efs = ratedIso * (2.0 ** ((eRef - e_speed) / 0.3))

         -- N = MR * (1.0 - LER / (0.3 * G * normalSbr))
         let nVal = mr * (1.0 - ler / (0.3 * g * normalSbr))

         return $ FilmCurveStats t g e_speed efs nVal

  mapM computeStats filmCurves

-- | Pure Haskell helper to fit simple linear regression y = a + b * x
fitLinearSimple :: [Double] -> [Double] -> [Double]
fitLinearSimple xs ys =
  let n = fromIntegral (length xs)
      sumX = sum xs
      sumY = sum ys
      sumXX = sum (map (\x -> x * x) xs)
      sumXY = sum (zipWith (*) xs ys)

      det = n * sumXX - sumX * sumX
  in if abs det < 1e-12
     then [sumY / n, 0.0]
     else
       let a = (sumY * sumXX - sumX * sumXY) / det
           b = (n * sumXY - sumX * sumY) / det
       in [a, b]

-- | Pure Haskell helper to fit simple quadratic regression y = a + b * x + c * x^2
fitQuadraticSimple :: [Double] -> [Double] -> [Double]
fitQuadraticSimple xs ys =
  let n = fromIntegral (length xs)
      sumX = sum xs
      sumXX = sum (map (\x -> x * x) xs)
      sumXXX = sum (map (\x -> x * x * x) xs)
      sumXXXX = sum (map (\x -> x * x * x * x) xs)
      sumY = sum ys
      sumXY = sum (zipWith (*) xs ys)
      sumXXY = sum (zipWith (\x y -> x * x * y) xs ys)

      m00 = n
      m01 = sumX
      m02 = sumXX
      m10 = sumX
      m11 = sumXX
      m12 = sumXXX
      m20 = sumXX
      m21 = sumXXX
      m22 = sumXXXX

      det = m00 * (m11 * m22 - m12 * m21) -
            m01 * (m10 * m22 - m12 * m20) +
            m02 * (m10 * m21 - m11 * m20)
  in if abs det < 1e-12
     then [sumY / n, 0.0, 0.0]
     else
       let d0 = sumY   * (m11 * m22 - m12 * m21) -
                m01    * (sumXY * m22 - m12 * sumXXY) +
                m02    * (sumXY * m21 - m11 * sumXXY)

           d1 = m00    * (sumXY * m22 - m12 * sumXXY) -
                sumY   * (m10 * m22 - m12 * m20) +
                m02    * (m10 * sumXXY - sumXY * m20)

           d2 = m00    * (m11 * sumXXY - sumXY * m21) -
                m01    * (m10 * sumXXY - sumXY * m20) +
                sumY   * (m10 * m21 - m11 * m20)
       in [d0 / det, d1 / det, d2 / det]

-- | Fit analytical polynomial regression models to the film curve stats points.
fitFieldChartModel :: [FilmCurveStats] -> FieldChartModel
fitFieldChartModel stats =
  let nVals = map statsNValue stats
      tVals = map (log . realToFrac . statsDevTime) stats
      sVals = map statsIsoSpeed stats

      timeCoeffs = fitLinearSimple nVals tVals
      speedCoeffs = fitQuadraticSimple nVals sVals
  in FieldChartModel timeCoeffs speedCoeffs

-- | Evaluate the fitted time model T(N) at a target N-value.
evaluateTimeModel :: FieldChartModel -> Double -> Float
evaluateTimeModel model nVal =
  case timeModelParams model of
    [a, b] -> realToFrac $ exp (a + b * nVal)
    _ -> error "evaluateTimeModel: expected 2 parameters"

-- | Evaluate the fitted speed model S(N) at a target N-value.
evaluateSpeedModel :: FieldChartModel -> Double -> Double
evaluateSpeedModel model nVal =
  case speedModelParams model of
    [a, b, c] -> a + b * nVal + c * nVal * nVal
    _ -> error "evaluateSpeedModel: expected 3 parameters"
