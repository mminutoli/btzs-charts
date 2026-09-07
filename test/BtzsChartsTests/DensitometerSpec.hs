{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : BtzsChartsTests.DensitometerSpec
Description : Tests for BtzsCharts.Densitometer module.
Copyright   : (c) Marco Minutoli, 2026

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX
-}
module BtzsChartsTests.DensitometerSpec (btzsChartsDensitometerTests) where

import BtzsCharts.Densitometer

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Text.Printf (printf)

btzsChartsDensitometerTests :: TestTree
btzsChartsDensitometerTests = testGroup "Tests for BtzsCharts.Densitometer"
  [ testCase "Parse standard transmission reading" $ do
      parseDensitometerReading "T+0.04D\r\n" @?= Right (DensitometerReading Transmission 0.04)
      parseDensitometerReading "T+1.52D\n"    @?= Right (DensitometerReading Transmission 1.52)
      parseDensitometerReading "T+0.00D"      @?= Right (DensitometerReading Transmission 0.0)

  , testCase "Parse standard reflection reading" $ do
      parseDensitometerReading "R+0.69D\r\n" @?= Right (DensitometerReading Reflection 0.69)
      parseDensitometerReading "R+2.15D\n"    @?= Right (DensitometerReading Reflection 2.15)
      parseDensitometerReading "R+0.00D"      @?= Right (DensitometerReading Reflection 0.0)

  , testCase "Parse negative density reading" $ do
      parseDensitometerReading "T-0.02D\r\n" @?= Right (DensitometerReading Transmission (-0.02))
      parseDensitometerReading "R-0.01D"      @?= Right (DensitometerReading Reflection (-0.01))

  , testCase "Reject invalid formats" $ do
      assertBool "Empty string" (isLeft (parseDensitometerReading ""))
      assertBool "Missing mode" (isLeft (parseDensitometerReading "+0.04D"))
      assertBool "Unknown mode" (isLeft (parseDensitometerReading "X+0.04D"))
      assertBool "Missing D suffix" (isLeft (parseDensitometerReading "T+0.04"))
      assertBool "Non-numeric value" (isLeft (parseDensitometerReading "T+abcD"))

  , testProperty "Roundtrip property for generated readings" prop_densitometer_roundtrip
  ]
  where
    isLeft (Left _) = True
    isLeft _        = False

-- | Property: Any generated mode and 2-decimal density value formats and parses back accurately.
prop_densitometer_roundtrip :: Property
prop_densitometer_roundtrip = property $ do
  isTrans <- forAll Gen.bool
  let mType = if isTrans then Transmission else Reflection
      modeChar = if isTrans then 'T' else 'R'

  val <- forAll $ Gen.double (Range.linearFrac (-0.1) 4.0)
  -- Round to 2 decimal places to match Printalyzer output resolution
  let roundedVal = fromIntegral (round (val * 100) :: Integer) / 100.0
      signChar = if roundedVal < 0 then '-' else '+'
      absVal = abs roundedVal
      rawLine = printf "%c%c%.2fD\r\n" modeChar signChar absVal

  case parseDensitometerReading rawLine of
    Left err -> do
      annotate err
      failure
    Right (DensitometerReading parsedType parsedVal) -> do
      parsedType === mType
      diff parsedVal (\a b -> abs (a - b) < 1e-4) roundedVal
