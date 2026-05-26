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
import BtzsChartsTests.Generators

import Control.Monad.Reader
import Hedgehog
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
