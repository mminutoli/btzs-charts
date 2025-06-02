{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : BtzsChartsTests.TypesSpec
Description : Tests for the module BtzsCharts.Types.
Copyright   : (c) Marco Minutoli, 2025

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX

This module tests the properties and functionalities of the BtzsCharts.Types.
-}

module BtzsChartsTests.TypesSpec(btzsChartsTypesTests) where

import BtzsCharts.Types

import Data.Aeson ( ToJSON(toJSON), object, KeyValue((.=)) )
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.Hedgehog ( testProperty )

import qualified Data.Vector as V

btzsChartsTypesTests :: TestTree
btzsChartsTypesTests = testGroup "Tests for BtzsCharts.Types"
  [ testProperty "JSON representation of StepTablet" prop_StepTablet_ToJSON_representation
  , testProperty "JSON representation of MaterialTest" prop_MaterialTest_ToJSON_representation
  ]

-- | Generate densities.
genDensity :: Gen Density
genDensity = Gen.float (Range.linearFrac 0.0 3.0)

-- | Generate array of densities.
genDensityReadings :: Gen DensityReadings
genDensityReadings = V.fromList <$> Gen.list (Range.linear 1 31) genDensity

-- | Generate StepTablet objects.
genStepTablet :: Gen StepTablet
genStepTablet = StepTablet
  <$> Gen.text (Range.constant 0 100) Gen.alphaNum
  <*> genDensityReadings

-- | Property to test the ToJSON representation of a StepTablet.
prop_StepTablet_ToJSON_representation :: Property
prop_StepTablet_ToJSON_representation = property $ do
  aStepTablet <- forAll genStepTablet
  let
    -- Converted Value of the StepTablet.
    jsonValue = toJSON aStepTablet

    -- The expected representation of the StepTablet.
    expectedValue = object [ "steptabletName" .= steptabletName aStepTablet
                           , "densities" .= densities aStepTablet ]

  -- The representation matches the expected one.
  jsonValue === expectedValue

-- | Generate MaterialTest.
genMaterialTest :: Gen MaterialTest
genMaterialTest = MaterialTest
  <$> Gen.text (Range.constant 0 100) Gen.alphaNum
  <*> Gen.text (Range.constant 0 100) Gen.alphaNum
  <*> Gen.float (Range.linearFrac 0 30)
  <*> Gen.map
        (Range.linear 5 10)
        ((,) <$> Gen.float (Range.linearFrac 0 30)
             <*> genDensityReadings)

-- | Property to test ToJSON representation of MaterialTest.
prop_MaterialTest_ToJSON_representation :: Property
prop_MaterialTest_ToJSON_representation = property $ do
  aMaterialTest <- forAll genMaterialTest
  let
    -- Convert Value of the MaterialTest
    jsonValue = toJSON aMaterialTest

    -- The expected representation of the MaterialTest
    expectedValue = object [ "name" .= name aMaterialTest
                           , "developer" .= developer aMaterialTest
                           , "temperature" .= temperature aMaterialTest
                           , "results" .= results aMaterialTest ]

  -- The representation matches the expected one.
  jsonValue === expectedValue
