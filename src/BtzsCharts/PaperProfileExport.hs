{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : BtzsCharts.PaperProfileExport
Description : Exporter logic for creating Paper Profiles for the ESP32 timer.
Copyright   : (c) Marco Minutoli, 2026

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX

This module fits characteristic curves of photographic paper to the logistic
model and exports the parameters as a JSON profile for use on the ESP32 timer.
-}
module BtzsCharts.PaperProfileExport
  ( PaperGradeProfile(..)
  , PaperProfile(..)
  , buildTargetProfile
  ) where

import BtzsCharts.HDCurveFitting
import BtzsCharts.Types

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Vector.Storable as VS

-- | Represents parameters of a single paper grade.
data PaperGradeProfile = PaperGradeProfile
  { label :: !T.Text
  , dMin :: !Double
  , dMax :: !Double
  , x0 :: !Double
  , slope :: !Double
  }
  deriving stock (Generic, Show)

customOptions :: Options
customOptions = defaultOptions
  { fieldLabelModifier = \f -> case f of
      "dMin" -> "D_min"
      "dMax" -> "D_max"
      other  -> other
  }

instance ToJSON PaperGradeProfile where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON PaperGradeProfile where
  parseJSON = genericParseJSON customOptions

-- | Represents the full paper profile JSON payload.
data PaperProfile = PaperProfile
  { name :: !T.Text
  , developer :: !T.Text
  , temperature :: !Float
  , grades :: ![PaperGradeProfile]
  }
  deriving stock (Generic, Show)

instance ToJSON PaperProfile
instance FromJSON PaperProfile

-- | Formats a grade development time float back into standard grade labels.
formatGrade :: Float -> T.Text
formatGrade v
  | v == -0.5 = "00"
  | v == 0.0  = "0"
  | otherwise =
      let r = round v
      in if abs (v - fromIntegral r) < 1e-5
         then T.pack (show r)
         else T.pack (show v)

-- | Fit paper curves and build the paper profile JSON payload.
buildTargetProfile :: StepTablet -> MaterialTest -> PaperProfile
buildTargetProfile stepTablet mt = case mt of
  PaperTest (PaperTestData n dev temp _ lux exp) ->
    let curves = fitHDCurves stepTablet mt
        stepWedgeDensities = densities stepTablet
        maxDensity = if VS.null stepWedgeDensities
                     then 3.0
                     else VS.maximum stepWedgeDensities
        buildGrade hd =
          case modelParameters hd of
            [dMinVal, dMaxVal, slopeVal, inflVal] ->
              let x0_val = case (lux, exp) of
                    (Just l, Just e) ->
                      inflVal + logBase 10 (l * e) - maxDensity
                    _ -> inflVal
              in PaperGradeProfile
                   { label = formatGrade (developmentTime hd)
                   , dMin = dMinVal
                   , dMax = dMaxVal
                   , x0 = x0_val
                   , slope = slopeVal
                   }
            _ -> error "buildTargetProfile: expected 4 model parameters"
    in PaperProfile
         { name = n
         , developer = dev
         , temperature = temp
         , grades = Prelude.map buildGrade curves
         }
  FilmTest _ ->
    error "buildTargetProfile: expected PaperTest, got FilmTest"
