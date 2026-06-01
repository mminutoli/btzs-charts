{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : BtzsChartsTests.TypesSpec
Description : Tests for the module BtzsCharts.Types.
Copyright   : (c) Marco Minutoli, 2025

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX
-}

module BtzsChartsTests.TypesSpec (btzsChartsTypesTests) where

import BtzsCharts.Types
import BtzsChartsTests.Generators ()

import Data.Aeson
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.Hedgehog ( testProperty )

import qualified Data.Vector.Storable as VS

btzsChartsTypesTests :: TestTree
btzsChartsTypesTests = testGroup "Tests for BtzsCharts.Types"
  [ testProperty "JSON representation of StepTablet" prop_StepTablet_ToJSON_representation
  , testProperty "JSON representation of MaterialTest" prop_MaterialTest_ToJSON_representation ]

-- | Generate a random DensityReadings.
genDensityReadings :: Gen DensityReadings
genDensityReadings = VS.fromList <$> Gen.list (Range.linear 5 10) (Gen.double $ Range.linearFrac 0 3)

-- | Generate StepTablet.
genStepTablet :: Gen StepTablet
genStepTablet = StepTablet
  <$> Gen.text (Range.constant 0 100) Gen.alphaNum
  <*> genDensityReadings

-- | Property to test ToJSON representation of StepTablet.
prop_StepTablet_ToJSON_representation :: Property
prop_StepTablet_ToJSON_representation = property $ do
  aStepTablet <- forAll genStepTablet
  let
    -- Convert Value of the StepTablet
    jsonValue = toJSON aStepTablet

    -- The expected representation of the StepTablet
    expectedValue = object [ "steptabletName" .= steptabletName aStepTablet
                           , "densities" .= densities aStepTablet ]

  -- The representation matches the expected one.
  jsonValue === expectedValue

-- | Generate MaterialTest.
genMaterialTest :: Gen MaterialTest
genMaterialTest = Gen.choice [genFilm, genPaper]
  where
    genFilm = do
      n <- Gen.text (Range.constant 0 100) Gen.alphaNum
      d <- Gen.text (Range.constant 0 100) Gen.alphaNum
      t <- Gen.float (Range.linearFrac 0 30)
      iso <- Gen.double (Range.linearFrac 50 400)
      m <- Gen.map (Range.linear 5 10)
             ((,) <$> Gen.float (Range.linearFrac 0 30) <*> genDensityReadings)
      return $ FilmTest (FilmTestData n d t iso m)
    genPaper = do
      n <- Gen.text (Range.constant 0 100) Gen.alphaNum
      d <- Gen.text (Range.constant 0 100) Gen.alphaNum
      t <- Gen.float (Range.linearFrac 0 30)
      m <- Gen.map (Range.linear 5 10)
             ((,) <$> Gen.text (Range.constant 1 2) Gen.alphaNum <*> genDensityReadings)
      return $ PaperTest (PaperTestData n d t m)

-- | Property to test ToJSON representation of MaterialTest.
prop_MaterialTest_ToJSON_representation :: Property
prop_MaterialTest_ToJSON_representation = property $ do
  aMaterialTest <- forAll genMaterialTest
  let
    -- Convert Value of the MaterialTest
    jsonValue = toJSON aMaterialTest

    -- The expected representation of the MaterialTest
    expectedValue = case aMaterialTest of
      FilmTest (FilmTestData n d t iso m) ->
        object [ "type" .= ("Film" :: String)
               , "name" .= n
               , "developer" .= d
               , "temperature" .= t
               , "ratedIso" .= iso
               , "measurements" .= m ]
      PaperTest (PaperTestData n d t m) ->
        object [ "type" .= ("Paper" :: String)
               , "name" .= n
               , "developer" .= d
               , "temperature" .= t
               , "measurements" .= m ]

  -- The representation matches the expected one.
  jsonValue === expectedValue
