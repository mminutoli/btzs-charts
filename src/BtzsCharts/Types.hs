{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : BtzsCharts.Types
Description : Types capturing the sensitometric experiments on film.
Copyright   : (c) Marco Minutoli, 2025

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX

Types capturing the information from the Paper and Film Testing procedures.
-}

module BtzsCharts.Types (
  Density,
  DensityReadings,
  MeasurementSeries(..),
  MaterialTest(..),
  FilmTestData(..),
  PaperTestData(..),
  StepTablet(..),
  ZoneOrientation(..),
  ZoneSystemConfig(..),
  PaperProcessConfig(..),
  FilmProcessConfig(..),
  ProcessConfiguration(..),
  zoneCenterFraction,
  zoneContrastConstant,
  normalSbrStops,
  effectiveNormalSbr,
  resolveHighlightFraction,
  resolveShadowFraction,
  resolveHlFraction,
  resolveShFraction,
  HasStandardAvgGradient(..),
  HasSpeedPointFactor(..),
  HasFlareCompensationFactor(..),
  HasFilmSpeedPointDensity(..),
  zoneRange,
  paperSpeedPointDensity,
  paperIdMaxPercentage,
  validateMeasurements,
  ProcessConfM
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import GHC.Generics ( Generic )
import Control.Monad.Reader

import qualified Data.Text   as T
import qualified Data.Map    as M
import qualified Data.Vector.Storable as VS

-- | A density as read by the densitometer.
type Density = Double

-- | A series of desities as read from the experiment.
type DensityReadings = VS.Vector Density

-- | A single measurement curve/series (density readings + optional illuminance in lux).
data MeasurementSeries = MeasurementSeries
  { seriesReadings :: !DensityReadings
  , seriesLux      :: !(Maybe Double)
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON MeasurementSeries where
  parseJSON v = parseObject v <|> parseArray v
    where
      parseObject = withObject "MeasurementSeries" $ \o ->
        MeasurementSeries
          <$> o .: "densities"
          <*> o .:? "lux"
      parseArray val = MeasurementSeries
        <$> parseJSON val
        <*> pure Nothing

instance ToJSON MeasurementSeries where
  toJSON (MeasurementSeries ds Nothing) = toJSON ds
  toJSON (MeasurementSeries ds (Just l)) =
    object [ "densities" .= ds, "lux" .= l ]

-- | Step Tablet
data StepTablet =
  StepTablet
  {
    -- | Step Tablet Name
    steptabletName :: !T.Text,
    -- | Densities as read by the densitometers.
    densities :: !DensityReadings
  }
  deriving stock (Generic, Show)

instance ToJSON StepTablet
instance FromJSON StepTablet

-- | Data specific to a film test.
data FilmTestData = FilmTestData
  { filmName :: !T.Text
  , filmDeveloper :: !T.Text
  , filmTemperature :: !Float
  , filmRatedIso :: !Double
  , filmMeasurements :: !(M.Map Float MeasurementSeries)
  , filmLux :: !(Maybe Double)
  , filmExposureTime :: !(Maybe Double)
  }
  deriving stock (Generic, Show, Eq)

-- | Data specific to a paper test.
data PaperTestData = PaperTestData
  { paperName :: !T.Text
  , paperDeveloper :: !T.Text
  , paperTemperature :: !Float
  , paperMeasurements :: !(M.Map T.Text MeasurementSeries)
  , paperLux :: !(Maybe Double)
  , paperExposureTime :: !(Maybe Double)
  }
  deriving stock (Generic, Show, Eq)

-- | Capture the settings and the results of testing materials.
data MaterialTest
  = FilmTest FilmTestData
  | PaperTest PaperTestData
  deriving stock (Generic, Show, Eq)

instance FromJSON MaterialTest where
  parseJSON = withObject "MaterialTest" $ \v -> do
    t <- v .: "type"
    case t of
      "Film" -> FilmTest <$> parseFilm v
      "Paper" -> PaperTest <$> parsePaper v
      _ -> fail $ "Unknown MaterialTest type: " ++ t
    where
      parseFilm v = FilmTestData
        <$> v .: "name"
        <*> v .: "developer"
        <*> v .: "temperature"
        <*> v .: "ratedIso"
        <*> v .: "measurements"
        <*> v .:? "lux"
        <*> v .:? "exposureTime"
      parsePaper v = PaperTestData
        <$> v .: "name"
        <*> v .: "developer"
        <*> v .: "temperature"
        <*> v .: "measurements"
        <*> v .:? "lux"
        <*> v .:? "exposureTime"

instance ToJSON MaterialTest where
  toJSON (FilmTest (FilmTestData n d t iso m lux expTime)) =
    object $ [ "type" .= ("Film" :: String)
             , "name" .= n
             , "developer" .= d
             , "temperature" .= t
             , "ratedIso" .= iso
             , "measurements" .= m
             ] ++ maybe [] (\l -> ["lux" .= l]) lux
               ++ maybe [] (\e -> ["exposureTime" .= e]) expTime
  toJSON (PaperTest (PaperTestData n d t m lux expTime)) =
    object $ [ "type" .= ("Paper" :: String)
             , "name" .= n
             , "developer" .= d
             , "temperature" .= t
             , "measurements" .= m
             ] ++ maybe [] (\l -> ["lux" .= l]) lux
               ++ maybe [] (\e -> ["exposureTime" .= e]) expTime

-- | Validate that the material test data exactly matches the length of the step tablet used.
validateMeasurements :: StepTablet -> MaterialTest -> Either String ()
validateMeasurements tablet test =
  let expectedLen = VS.length (densities tablet)
      rs = case test of
             FilmTest d -> map seriesReadings (M.elems (filmMeasurements d))
             PaperTest d -> map seriesReadings (M.elems (paperMeasurements d))
  in if all (\r -> VS.length r == expectedLen) rs
     then Right ()
     else Left "Mismatch between StepTablet length and measurement lengths."

-- | Orientation of the Zone System partitioning.
data ZoneOrientation
  = BlackIsZone0  -- ^ Standard print convention: Zone 0 is Dmax (black), Zone N-1 is Dmin (white).
  | WhiteIsZone0  -- ^ Transmission / inverted convention: Zone 0 is Dmin (white), Zone N-1 is Dmax (black).
  deriving stock (Generic, Show, Eq)

instance FromJSON ZoneOrientation where
  parseJSON = withText "ZoneOrientation" $ \t -> case t of
    "blackIsZone0" -> pure BlackIsZone0
    "whiteIsZone0" -> pure WhiteIsZone0
    other -> fail $ "Unknown zone orientation: " ++ T.unpack other

instance ToJSON ZoneOrientation where
  toJSON BlackIsZone0 = "blackIsZone0"
  toJSON WhiteIsZone0 = "whiteIsZone0"

-- | Configuration for the Zone System partitioning.
data ZoneSystemConfig = ZoneSystemConfig
  { numZones          :: !Int
  , orientation       :: !ZoneOrientation
  , highlightZone     :: !Double
  , shadowZone        :: !Double
  , customZoneCenters :: !(M.Map T.Text Double)
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON ZoneSystemConfig where
  parseJSON = withObject "ZoneSystemConfig" $ \o -> ZoneSystemConfig
    <$> o .:? "numZones" .!= 10
    <*> o .:? "orientation" .!= BlackIsZone0
    <*> o .: "highlightZone"
    <*> o .: "shadowZone"
    <*> o .:? "customZoneCenters" .!= M.empty

instance ToJSON ZoneSystemConfig where
  toJSON (ZoneSystemConfig nz ori hl sh czc) = object $
    [ "numZones" .= nz
    , "orientation" .= ori
    , "partitionMethod" .= ("uniformDensity" :: T.Text)
    , "highlightZone" .= hl
    , "shadowZone" .= sh
    ] ++ [ "customZoneCenters" .= czc | not (M.null czc) ]

-- | Process configuration parameters specific to photographic paper.
data PaperProcessConfig = PaperProcessConfig
  { paperTargetGrade         :: !T.Text
  , highlightDensityFraction :: !(Maybe Double)
  , shadowDensityFraction    :: !(Maybe Double)
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON PaperProcessConfig where
  parseJSON = withObject "PaperProcessConfig" $ \o -> PaperProcessConfig
    <$> o .:? "targetGrade" .!= "2"
    <*> o .:? "highlightDensityFraction"
    <*> o .:? "shadowDensityFraction"

instance ToJSON PaperProcessConfig where
  toJSON (PaperProcessConfig tg hl sh) = object $
    [ "targetGrade" .= tg ]
    ++ maybe [] (\h -> [ "highlightDensityFraction" .= h ]) hl
    ++ maybe [] (\s -> [ "shadowDensityFraction" .= s ]) sh

-- | Process configuration parameters specific to photographic film.
data FilmProcessConfig = FilmProcessConfig
  { filmSpeedPointOffset        :: !Double
  , filmStandardAvgGradient     :: !Double
  , filmSpeedPointFactor        :: !Double
  , filmFlareCompensationFactor :: !Double
  , filmNormalSbrStops          :: !(Maybe Double)
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON FilmProcessConfig where
  parseJSON = withObject "FilmProcessConfig" $ \o -> FilmProcessConfig
    <$> o .:? "speedPointDensity" .!= 0.10
    <*> o .:? "standardAvgGradient" .!= 0.58
    <*> o .:? "speedPointFactor" .!= 1.0
    <*> o .:? "flareCompensationFactor" .!= 1.03
    <*> o .:? "normalSbrStops"

instance ToJSON FilmProcessConfig where
  toJSON (FilmProcessConfig spd sag spf fcf sbr) = object $
    [ "speedPointDensity" .= spd
    , "standardAvgGradient" .= sag
    , "speedPointFactor" .= spf
    , "flareCompensationFactor" .= fcf
    ] ++ maybe [] (\s -> [ "normalSbrStops" .= s ]) sbr

-- | Unified Process Configuration capturing zone system, paper, and film parameters.
data ProcessConfiguration = ProcessConfiguration
  { processVersion        :: !T.Text
  , processName           :: !T.Text
  , zoneSystem            :: !ZoneSystemConfig
  , paperConfig           :: !PaperProcessConfig
  , filmConfig            :: !FilmProcessConfig
  , legacyPaperSpeedPoint :: !(Maybe Double)
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON ProcessConfiguration where
  parseJSON = withObject "ProcessConfiguration" $ \o -> do
    mZs <- o .:? "zoneSystem"
    case mZs of
      Just zs -> do
        ver <- o .:? "version" .!= "2.0"
        nm  <- o .:? "name" .!= "Standard Process"
        pap <- o .:? "paper" .!= PaperProcessConfig "2" Nothing Nothing
        flm <- o .: "film"
        return $ ProcessConfiguration ver nm zs pap flm Nothing
      Nothing -> do
        g       <- o .:? "standardAvgGradient" .!= 0.58
        spf     <- o .:? "speedPointFactor" .!= 1.0
        fcf     <- o .:? "flareCompensationFactor" .!= 1.03
        zr      <- o .:? "zoneRange" .!= 7.0
        fspd    <- o .:? "filmSpeedPointDensity" .!= 0.10
        mPspd   <- o .:? "paperSpeedPointDensity"
        mPidmax <- o .:? "paperIdMaxPercentage"

        let defaultZs = ZoneSystemConfig
              { numZones          = 10
              , orientation       = BlackIsZone0
              , highlightZone     = 9.0
              , shadowZone        = 2.0
              , customZoneCenters = M.empty
              }
            defaultPaper = PaperProcessConfig
              { paperTargetGrade         = "2"
              , highlightDensityFraction = Nothing
              , shadowDensityFraction    = mPidmax
              }
            defaultFilm = FilmProcessConfig
              { filmSpeedPointOffset        = fspd
              , filmStandardAvgGradient     = g
              , filmSpeedPointFactor        = spf
              , filmFlareCompensationFactor = fcf
              , filmNormalSbrStops          = Just zr
              }
        return $ ProcessConfiguration "1.0" "Legacy 1.0 Process" defaultZs defaultPaper defaultFilm mPspd

instance ToJSON ProcessConfiguration where
  toJSON cfg = object
    [ "version" .= processVersion cfg
    , "name" .= processName cfg
    , "zoneSystem" .= zoneSystem cfg
    , "paper" .= paperConfig cfg
    , "film" .= filmConfig cfg
    ]

-- | Helper to get normalized center fraction for zone k.
zoneCenterFraction :: ZoneSystemConfig -> Double -> Double
zoneCenterFraction zsc k =
  let keyInt = T.pack (show (round k :: Int))
      keyDbl = T.pack (show k)
      n = fromIntegral (numZones zsc)
      defaultCenter = case orientation zsc of
        BlackIsZone0 -> 1.0 - (k + 0.5) / n
        WhiteIsZone0 -> (k + 0.5) / n
  in case M.lookup keyInt (customZoneCenters zsc) of
       Just y  -> y
       Nothing -> case M.lookup keyDbl (customZoneCenters zsc) of
         Just y  -> y
         Nothing -> defaultCenter

-- | Zone Contrast Constant K_zone = ln((y_sh * (1 - y_hl)) / (y_hl * (1 - y_sh)))
zoneContrastConstant :: Double -> Double -> Double
zoneContrastConstant yHl ySh =
  log ((ySh * (1.0 - yHl)) / (yHl * (1.0 - ySh)))

-- | Normal SBR in stops directly from |k_highlight - k_shadow|.
normalSbrStops :: ZoneSystemConfig -> Double
normalSbrStops zsc = abs (highlightZone zsc - shadowZone zsc)

-- | Effective normal SBR in stops, respecting film normalSbrStops override if set.
effectiveNormalSbr :: ProcessConfiguration -> Double
effectiveNormalSbr cfg = case filmNormalSbrStops (filmConfig cfg) of
  Just sbr -> sbr
  Nothing  -> normalSbrStops (zoneSystem cfg)

-- | Resolve effective highlight density fraction y_hl from ProcessConfiguration.
resolveHighlightFraction :: ProcessConfiguration -> Double
resolveHighlightFraction cfg =
  case highlightDensityFraction (paperConfig cfg) of
    Just y  -> y
    Nothing -> zoneCenterFraction (zoneSystem cfg) (highlightZone (zoneSystem cfg))

-- | Resolve effective shadow density fraction y_sh from ProcessConfiguration.
resolveShadowFraction :: ProcessConfiguration -> Double
resolveShadowFraction cfg =
  case shadowDensityFraction (paperConfig cfg) of
    Just y  -> y
    Nothing -> zoneCenterFraction (zoneSystem cfg) (shadowZone (zoneSystem cfg))

-- | Resolve highlight density fraction, taking into account Dmin, Dmax for legacy v1 configs.
resolveHlFraction :: ProcessConfiguration -> Double -> Double -> Double
resolveHlFraction cfg dMin dMax =
  case highlightDensityFraction (paperConfig cfg) of
    Just y -> y
    Nothing -> case legacyPaperSpeedPoint cfg of
      Just dSpd | dMax > dMin -> dSpd / (dMax - dMin)
      _ -> zoneCenterFraction (zoneSystem cfg) (highlightZone (zoneSystem cfg))

-- | Resolve shadow density fraction from ProcessConfiguration.
resolveShFraction :: ProcessConfiguration -> Double
resolveShFraction = resolveShadowFraction

-- | Backward-compatible accessor for standardAvgGradient.
class HasStandardAvgGradient a where
  standardAvgGradient :: a -> Double

instance HasStandardAvgGradient FilmProcessConfig where
  standardAvgGradient = filmStandardAvgGradient

instance HasStandardAvgGradient ProcessConfiguration where
  standardAvgGradient = filmStandardAvgGradient . filmConfig

-- | Backward-compatible accessor for speedPointFactor.
class HasSpeedPointFactor a where
  speedPointFactor :: a -> Double

instance HasSpeedPointFactor FilmProcessConfig where
  speedPointFactor = filmSpeedPointFactor

instance HasSpeedPointFactor ProcessConfiguration where
  speedPointFactor = filmSpeedPointFactor . filmConfig

-- | Backward-compatible accessor for flareCompensationFactor.
class HasFlareCompensationFactor a where
  flareCompensationFactor :: a -> Double

instance HasFlareCompensationFactor FilmProcessConfig where
  flareCompensationFactor = filmFlareCompensationFactor

instance HasFlareCompensationFactor ProcessConfiguration where
  flareCompensationFactor = filmFlareCompensationFactor . filmConfig

-- | Backward-compatible accessor for filmSpeedPointDensity.
class HasFilmSpeedPointDensity a where
  filmSpeedPointDensity :: a -> Double

instance HasFilmSpeedPointDensity FilmProcessConfig where
  filmSpeedPointDensity = filmSpeedPointOffset

instance HasFilmSpeedPointDensity ProcessConfiguration where
  filmSpeedPointDensity = filmSpeedPointOffset . filmConfig

-- | Backward-compatible accessor for zoneRange.
zoneRange :: ProcessConfiguration -> Double
zoneRange = effectiveNormalSbr

-- | Backward-compatible accessor for paperSpeedPointDensity.
paperSpeedPointDensity :: ProcessConfiguration -> Double
paperSpeedPointDensity cfg = case legacyPaperSpeedPoint cfg of
  Just d  -> d
  Nothing -> 0.04

-- | Backward-compatible accessor for paperIdMaxPercentage.
paperIdMaxPercentage :: ProcessConfiguration -> Double
paperIdMaxPercentage cfg = case shadowDensityFraction (paperConfig cfg) of
  Just y  -> y
  Nothing -> 0.90

type ProcessConfM = Reader ProcessConfiguration
