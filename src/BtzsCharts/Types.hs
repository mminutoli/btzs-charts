{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
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
  MaterialTest(..),
  StepTablet(..),
  ProcessConfiguration(..),
  validateMeasurements,
  ProcessConfM
  ) where

import Data.Aeson
import Data.Aeson.Types (SumEncoding(..))
import GHC.Generics ( Generic )
import Control.Monad.Reader

import qualified Data.Text   as T
import qualified Data.Map    as M
import qualified Data.Vector.Storable as VS

-- | A density as read by the densitometer.
type Density = Double

-- | A series of desities as read from the experiment.
type DensityReadings = VS.Vector Density

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

-- | Capture the settings and the results of testing materials.
data MaterialTest
  = FilmTest
    { -- | The name of the tested material.
      name :: !T.Text
    , -- | The developer used.
      developer :: !T.Text
    , -- | the temperature of the development process.
      temperature :: !Float
    , -- | A map storing the results from testing.
      measurements :: !(M.Map Float DensityReadings)
    }
  | PaperTest
    { -- | The name of the tested material.
      name :: !T.Text
    , -- | The developer used.
      developer :: !T.Text
    , -- | the temperature of the development process.
      temperature :: !Float
    , -- | A map storing the results from testing.
      paperMeasurements :: !(M.Map T.Text DensityReadings)
    }
  deriving stock (Generic, Show)

aesonOptions :: Options
aesonOptions = defaultOptions
  { sumEncoding = defaultTaggedObject { tagFieldName = "type" }
  , constructorTagModifier = \c -> if c == "FilmTest" then "Film" else "Paper"
  , fieldLabelModifier = \f -> if f == "paperMeasurements" then "measurements" else f
  }

instance FromJSON MaterialTest where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON MaterialTest where
  toJSON = genericToJSON aesonOptions

-- | Validate that the material test data exactly matches the length of the step tablet used.
validateMeasurements :: StepTablet -> MaterialTest -> Either String ()
validateMeasurements tablet test =
  let expectedLen = VS.length (densities tablet)
      rs = case test of
             FilmTest _ _ _ meas -> M.elems meas
             PaperTest _ _ _ meas -> M.elems meas
  in if all (\r -> VS.length r == expectedLen) rs
     then Right ()
     else Left "Mismatch between StepTablet length and measurement lengths."

data ProcessConfiguration =
  ProcessConfiguration
  {
    -- | The value to use as standard average gradient.
    standardAvgGradient :: !Double,
    -- | Factor used to compute the value of IDmin from the standard average gradient.
    speedPointFactor :: !Double,
    -- | Factor used to compensate for flare effects.
    flareCompensationFactor :: !Double,
    -- | Scale Index is a personalized value for the print exposure scale.
    scaleIndex :: !Double,
    -- | Number of zones to use to determine the N values
    zoneRange :: !Double,
    -- | Density above base+fog used to determine the film speed point (typically 0.1).
    filmSpeedPointDensity :: !Double,
    -- | Density above base+fog used to determine the paper speed point (typically 0.04).
    paperSpeedPointDensity :: !Double,
    -- | Percentage of Dmax used to determine IDmax for paper (typically 0.90 or 90%).
    paperIdMaxPercentage :: !Double
  }
  deriving stock (Generic, Show)

instance FromJSON ProcessConfiguration
instance ToJSON ProcessConfiguration

type ProcessConfM = Reader ProcessConfiguration
