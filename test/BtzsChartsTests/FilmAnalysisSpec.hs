{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : BtzsChartsTests.FilmAnalysisSpec
Description : Tests for the module BtzsCharts.FilmAnalysis.
Copyright   : (c) Marco Minutoli, 2025

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX
-}

module BtzsChartsTests.FilmAnalysisSpec (btzsChartsFilmAnalysisTests) where

import BtzsCharts.Types
import BtzsCharts.FilmAnalysis
import BtzsChartsTests.Generators

import Control.Monad.Reader
import Hedgehog
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.Hedgehog ( testProperty )

btzsChartsFilmAnalysisTests :: TestTree
btzsChartsFilmAnalysisTests = testGroup "Tests for BtzsCharts.FilmAnalysis"
  [ testProperty "Average gradient is positive for standard curves" prop_avgGradient_is_positive
  , testProperty "IDmax exposure is greater than IDmin exposure" prop_IDmax_gt_IDmin
  ]

-- | Property: avgGradient should be positive for our generated logistic curves.
prop_avgGradient_is_positive :: Property
prop_avgGradient_is_positive = property $ do
  conf <- forAll genProcessConfig
  curve <- forAll genHDCurve
  let g = runReader (avgGradient curve) conf
  assert (g > 0)

-- | Property: findIDmax should return a higher exposure (x) than findIDmin.
prop_IDmax_gt_IDmin :: Property
prop_IDmax_gt_IDmin = property $ do
  conf <- forAll genProcessConfig
  curve <- forAll genHDCurve
  let (x_min, _) = runReader (findIDmin curve) conf
  let (x_max, _) = runReader (findIDmax curve) conf
  assert (x_max > x_min)
