{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : BtzsChartsTests.PaperAnalysisSpec
Description : Tests for the module BtzsCharts.PaperAnalysis.
Copyright   : (c) Marco Minutoli, 2025

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX
-}

module BtzsChartsTests.PaperAnalysisSpec (btzsChartsPaperAnalysisTests) where

import BtzsCharts.Types
import BtzsCharts.HDCurveFitting
import BtzsCharts.PaperAnalysis

import Control.Monad.Reader
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.Hedgehog ( testProperty )

import qualified Data.Vector.Storable as VS

btzsChartsPaperAnalysisTests :: TestTree
btzsChartsPaperAnalysisTests = testGroup "Tests for BtzsCharts.PaperAnalysis"
  [ testProperty "Log Exposure Range is always positive" prop_LER_is_positive
  , testProperty "ISO Range is a multiple of 10" prop_ISO_Range_is_multiple_of_10
  , testProperty "Dynamic Range is within physical bounds" prop_DynamicRange_bounds
  , testProperty "Inverse model verification for Speed Point" prop_inverse_verification_speedpoint
  , testProperty "Inverse model verification for IDmax" prop_inverse_verification_idmax
  ]

-- | Generator for ProcessConfiguration.
genProcessConfig :: Gen ProcessConfiguration
genProcessConfig = ProcessConfiguration
  <$> Gen.double (Range.linearFrac 0.4 1.2)
  <*> Gen.double (Range.linearFrac 0.5 1.5)
  <*> Gen.double (Range.linearFrac 0.5 1.5)
  <*> Gen.double (Range.linearFrac 0.5 1.5)
  <*> Gen.double (Range.linearFrac 5.0 9.0)
  <*> Gen.double (Range.linearFrac 0.05 0.2)
  <*> Gen.double (Range.linearFrac 0.02 0.06)
  <*> Gen.double (Range.linearFrac 0.85 0.95)

-- | Generator for HDCurve using a logistic model.
genHDCurve :: Gen HDCurve
genHDCurve = do
  dMin  <- Gen.double (Range.linearFrac 0.03 0.15)
  dMax  <- Gen.double (Range.linearFrac 1.8 2.5)
  slope <- Gen.double (Range.linearFrac 1.5 5.0)
  infl  <- Gen.double (Range.linearFrac 0.5 2.0)
  let params = [dMin, dMax, slope, infl]
  let xs = VS.fromList [0.0, 0.1 .. 3.0]
  let ys = VS.fromList $ Prelude.concatMap (logisticModel params) (VS.toList xs)
  return $ HDCurve 20.0 xs ys params

-- | Property: Log Exposure Range must be strictly positive for a valid paper curve.
prop_LER_is_positive :: Property
prop_LER_is_positive = property $ do
  conf <- forAll genProcessConfig
  curve <- forAll genHDCurve
  let ler = runReader (logExposureRange curve) conf
  assert (ler > 0)

-- | Property: ISO Range should always be a multiple of 10.
prop_ISO_Range_is_multiple_of_10 :: Property
prop_ISO_Range_is_multiple_of_10 = property $ do
  conf <- forAll genProcessConfig
  curve <- forAll genHDCurve
  let iso = runReader (isoRange curve) conf
  (iso `mod` 10) === 0

-- | Property: Paper Dynamic Range should be between 0 and total Dmax range.
prop_DynamicRange_bounds :: Property
prop_DynamicRange_bounds = property $ do
  conf <- forAll genProcessConfig
  curve@(HDCurve _ _ _ params) <- forAll genHDCurve
  let [dMin, dMax, _, _] = params
  let dr = runReader (paperDynamicRange curve) conf
  assert (dr > 0 && dr < (dMax - dMin))

-- | Property: Passing the Speed Point Log Exposure into the model returns the target density.
prop_inverse_verification_speedpoint :: Property
prop_inverse_verification_speedpoint = property $ do
  conf <- forAll genProcessConfig
  curve <- forAll genHDCurve
  let (targetD, e) = runReader (paperSpeedPoint curve) conf
  let [calcD] = logisticModel (modelParameters curve) e
  diff calcD (\a b -> abs (a - b) < 1e-4) targetD

-- | Property: Passing the IDmax Log Exposure into the model returns the target density.
prop_inverse_verification_idmax :: Property
prop_inverse_verification_idmax = property $ do
  conf <- forAll genProcessConfig
  curve <- forAll genHDCurve
  let (targetD, e) = runReader (paperIdMax curve) conf
  let [calcD] = logisticModel (modelParameters curve) e
  diff calcD (\a b -> abs (a - b) < 1e-4) targetD
