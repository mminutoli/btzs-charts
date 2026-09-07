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
import BtzsChartsTests.Generators (genProcessConfig)

import Data.Aeson
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.Hedgehog ( testProperty )
import Test.Tasty.HUnit ( testCase, (@?=), assertBool, assertFailure, Assertion )

import qualified Data.Vector.Storable as VS

btzsChartsTypesTests :: TestTree
btzsChartsTypesTests = testGroup "Tests for BtzsCharts.Types"
  [ testProperty "JSON representation of StepTablet" prop_StepTablet_ToJSON_representation
  , testProperty "JSON representation of MaterialTest" prop_MaterialTest_ToJSON_representation
  , testProperty "JSON roundtrip of ProcessConfiguration" prop_ProcessConfig_roundtrip
  , testCase "10-Zone mathematics (centers, contrast constant, SBR)" test_10zone_math
  , testCase "11-Zone mathematics (centers, contrast constant, SBR)" test_11zone_math
  , testCase "Parse Standard_10Zone.json reference file" test_parse_standard_10zone
  , testCase "Parse Adams_11Zone.json reference file" test_parse_adams_11zone
  , testCase "Parse legacy v1 ProcessConfig.json with 10-zone defaults" test_parse_legacy_v1
  ]

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
    genSeries = MeasurementSeries
      <$> genDensityReadings
      <*> Gen.maybe (Gen.double (Range.linearFrac 0.1 1000.0))
    genFilm = do
      n <- Gen.text (Range.constant 0 100) Gen.alphaNum
      d <- Gen.text (Range.constant 0 100) Gen.alphaNum
      t <- Gen.float (Range.linearFrac 0 30)
      iso <- Gen.double (Range.linearFrac 50 400)
      m <- Gen.map (Range.linear 5 10)
             ((,) <$> Gen.float (Range.linearFrac 0 30) <*> genSeries)
      lux <- Gen.maybe (Gen.double (Range.linearFrac 0.1 1000.0))
      expTime <- Gen.maybe (Gen.double (Range.linearFrac 0.001 60.0))
      return $ FilmTest (FilmTestData n d t iso m lux expTime)
    genPaper = do
      n <- Gen.text (Range.constant 0 100) Gen.alphaNum
      d <- Gen.text (Range.constant 0 100) Gen.alphaNum
      t <- Gen.float (Range.linearFrac 0 30)
      m <- Gen.map (Range.linear 5 10)
             ((,) <$> Gen.text (Range.constant 1 2) Gen.alphaNum <*> genSeries)
      lux <- Gen.maybe (Gen.double (Range.linearFrac 0.1 1000.0))
      expTime <- Gen.maybe (Gen.double (Range.linearFrac 0.001 60.0))
      return $ PaperTest (PaperTestData n d t m lux expTime)

-- | Property to test ToJSON representation of MaterialTest.
prop_MaterialTest_ToJSON_representation :: Property
prop_MaterialTest_ToJSON_representation = property $ do
  aMaterialTest <- forAll genMaterialTest
  let
    -- Convert Value of the MaterialTest
    jsonValue = toJSON aMaterialTest

    -- The expected representation of the MaterialTest
    expectedValue = case aMaterialTest of
      FilmTest (FilmTestData n d t iso m lux expTime) ->
        object $ [ "type" .= ("Film" :: String)
                 , "name" .= n
                 , "developer" .= d
                 , "temperature" .= t
                 , "ratedIso" .= iso
                 , "measurements" .= m ]
                 ++ maybe [] (\l -> ["lux" .= l]) lux
                 ++ maybe [] (\e -> ["exposureTime" .= e]) expTime
      PaperTest (PaperTestData n d t m lux expTime) ->
        object $ [ "type" .= ("Paper" :: String)
                 , "name" .= n
                 , "developer" .= d
                 , "temperature" .= t
                 , "measurements" .= m ]
                 ++ maybe [] (\l -> ["lux" .= l]) lux
                 ++ maybe [] (\e -> ["exposureTime" .= e]) expTime

  -- The representation matches the expected one.
  jsonValue === expectedValue

-- | Property to test JSON roundtrip of ProcessConfiguration.
prop_ProcessConfig_roundtrip :: Property
prop_ProcessConfig_roundtrip = property $ do
  cfg <- forAll genProcessConfig
  let val = toJSON cfg
  case fromJSON val of
    Success cfg' -> do
      processVersion cfg' === processVersion cfg
      processName cfg' === processName cfg
      numZones (zoneSystem cfg') === numZones (zoneSystem cfg)
      orientation (zoneSystem cfg') === orientation (zoneSystem cfg)
      highlightZone (zoneSystem cfg') === highlightZone (zoneSystem cfg)
      shadowZone (zoneSystem cfg') === shadowZone (zoneSystem cfg)
      paperTargetGrade (paperConfig cfg') === paperTargetGrade (paperConfig cfg)
      standardAvgGradient cfg' === standardAvgGradient cfg
      speedPointFactor cfg' === speedPointFactor cfg
      flareCompensationFactor cfg' === flareCompensationFactor cfg
      filmSpeedPointDensity cfg' === filmSpeedPointDensity cfg
    Error err -> do
      annotate err
      failure

-- | Test Standard 10-Zone system mathematics.
test_10zone_math :: Assertion
test_10zone_math = do
  let zs = ZoneSystemConfig 10 BlackIsZone0 9.0 2.0 mempty
  assertBool "Zone 9 center close to 0.05" (abs (zoneCenterFraction zs 9.0 - 0.05) < 1e-7)
  assertBool "Zone 2 center close to 0.75" (abs (zoneCenterFraction zs 2.0 - 0.75) < 1e-7)
  assertBool "Zone 0 center close to 0.95" (abs (zoneCenterFraction zs 0.0 - 0.95) < 1e-7)
  assertBool "Zone 5 center close to 0.45" (abs (zoneCenterFraction zs 5.0 - 0.45) < 1e-7)
  normalSbrStops zs @?= 7.0
  let kZone = zoneContrastConstant (zoneCenterFraction zs 9.0) (zoneCenterFraction zs 2.0)
  assertBool "K_zone close to ln(57)" (abs (kZone - log 57.0) < 1e-7)
  assertBool "K_zone close to 4.043051" (abs (kZone - 4.0430512678) < 1e-5)

-- | Test Ansel Adams 11-Zone system mathematics.
test_11zone_math :: Assertion
test_11zone_math = do
  let zs = ZoneSystemConfig 11 BlackIsZone0 8.0 2.0 mempty
  assertBool "Zone VIII center is 2.5/11" (abs (zoneCenterFraction zs 8.0 - (2.5 / 11.0)) < 1e-7)
  assertBool "Zone II center is 8.5/11" (abs (zoneCenterFraction zs 2.0 - (8.5 / 11.0)) < 1e-7)
  normalSbrStops zs @?= 6.0
  let kZone = zoneContrastConstant (zoneCenterFraction zs 8.0) (zoneCenterFraction zs 2.0)
  assertBool "K_zone close to 2.44755" (abs (kZone - 2.447551) < 1e-4)

-- | Test parsing data/Standard_10Zone.json.
test_parse_standard_10zone :: Assertion
test_parse_standard_10zone = do
  res <- eitherDecodeFileStrict "data/Standard_10Zone.json"
  case res of
    Left err -> assertFailure $ "Failed to parse Standard_10Zone.json: " ++ err
    Right cfg -> do
      processVersion cfg @?= "2.0"
      processName cfg @?= "Standard 10-Zone BTZS Process"
      let zs = zoneSystem cfg
      numZones zs @?= 10
      orientation zs @?= BlackIsZone0
      highlightZone zs @?= 9.0
      shadowZone zs @?= 2.0
      normalSbrStops zs @?= 7.0
      effectiveNormalSbr cfg @?= 7.0
      standardAvgGradient cfg @?= 0.58
      speedPointFactor cfg @?= 1.0
      flareCompensationFactor cfg @?= 1.03
      filmSpeedPointDensity cfg @?= 0.10

-- | Test parsing data/Adams_11Zone.json.
test_parse_adams_11zone :: Assertion
test_parse_adams_11zone = do
  res <- eitherDecodeFileStrict "data/Adams_11Zone.json"
  case res of
    Left err -> assertFailure $ "Failed to parse Adams_11Zone.json: " ++ err
    Right cfg -> do
      processVersion cfg @?= "2.0"
      processName cfg @?= "Ansel Adams 11-Zone Process"
      let zs = zoneSystem cfg
      numZones zs @?= 11
      orientation zs @?= BlackIsZone0
      highlightZone zs @?= 8.0
      shadowZone zs @?= 2.0
      normalSbrStops zs @?= 6.0
      effectiveNormalSbr cfg @?= 6.0
      standardAvgGradient cfg @?= 0.58

-- | Test parsing legacy v1 data/ProcessConfig.json.
test_parse_legacy_v1 :: Assertion
test_parse_legacy_v1 = do
  res <- eitherDecodeFileStrict "data/ProcessConfig.json"
  case res of
    Left err -> assertFailure $ "Failed to parse legacy ProcessConfig.json: " ++ err
    Right cfg -> do
      processVersion cfg @?= "1.0"
      let zs = zoneSystem cfg
      numZones zs @?= 10
      orientation zs @?= BlackIsZone0
      highlightZone zs @?= 9.0
      shadowZone zs @?= 2.0
      normalSbrStops zs @?= 7.0
      zoneRange cfg @?= 7.0
      standardAvgGradient cfg @?= 0.58
      speedPointFactor cfg @?= 1.0
      flareCompensationFactor cfg @?= 1.03
      filmSpeedPointDensity cfg @?= 0.10
      paperSpeedPointDensity cfg @?= 0.04
      paperIdMaxPercentage cfg @?= 0.90
