{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright: (c) 2025 Marco Minutoli
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Marco Minutoli <mminutoli@gmail.com>
--
-- Types capturing the information from the Paper and Film Testing procedures.
module BtzsCharts.Types (
  Density,
  DensityReadings,
  MaterialTest(..)
  ) where

import Data.Aeson
import GHC.Generics

import qualified Data.Text   as T
import qualified Data.Map    as M
import qualified Data.Vector as VU

-- | A density as read by the densitometer.
type Density = Float

-- | A series of desities as read from the experiment.
type DensityReadings = VU.Vector Density

-- | Step Tablet
data StepTablet =
  StepTablet
  {
    -- | Step Tablet Name
    steptabletName :: T.Text,
    -- | Densities as read by the densitometers.
    densities :: DensityReadings
  }
  deriving (Generic, Show)

instance ToJSON StepTablet
instance FromJSON StepTablet

-- | Capture the settings and the results of testing materials.
data MaterialTest =
  MaterialTest
  {
    -- | The name of the tested material.
    name :: T.Text,
    -- | The paper developer used.
    developer :: T.Text,
    -- | the temperature of the development process.
    temperature :: Float,
    -- | A map storing the results from testing.
    results :: M.Map Float DensityReadings
  }
  deriving (Generic, Show)

instance FromJSON MaterialTest
instance ToJSON MaterialTest
