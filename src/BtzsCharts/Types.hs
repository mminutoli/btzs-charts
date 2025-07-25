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
  ProcessConfM
  ) where

import Data.Aeson ( FromJSON, ToJSON )
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
data MaterialTest =
  MaterialTest
  {
    -- | The name of the tested material.
    name :: !T.Text,
    -- | The developer used.
    developer :: !T.Text,
    -- | the temperature of the development process.
    temperature :: !Float,
    -- | A map storing the results from testing.
    results :: !(M.Map Float DensityReadings)
  }
  deriving stock (Generic, Show)

instance FromJSON MaterialTest
instance ToJSON MaterialTest

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
    zoneRange :: !Double
  }
  deriving stock (Generic, Show)

instance FromJSON ProcessConfiguration
instance ToJSON ProcessConfiguration

type ProcessConfM = Reader ProcessConfiguration
