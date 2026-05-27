{-# LANGUAGE OverloadedStrings #-}
module BtzsChartsTests.InterpolationSpec (btzsChartsInterpolationTests) where

import BtzsCharts.HDCurveFitting
import BtzsCharts.Interpolation
import BtzsChartsTests.Generators

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.Hedgehog ( testProperty )
import Data.List (sortOn)

btzsChartsInterpolationTests :: TestTree
btzsChartsInterpolationTests = testGroup "Tests for BtzsCharts.Interpolation"
  [ testProperty "Exact match interpolation returns the original curve" prop_exact_match
  , testProperty "Midpoint interpolation returns the arithmetic mean of parameters" prop_midpoint_interpolation
  ]

-- | Property: If target time exactly matches a measured curve, the result should be that curve.
prop_exact_match :: Property
prop_exact_match = property $ do
  c1 <- forAll genHDCurve
  c2 <- forAll genHDCurve
  -- Ensure times are distinct for a valid list
  let curves = sortOn developmentTime [c1 { developmentTime = 5 }, c2 { developmentTime = 10 }]
  let targetTime = 5.0
  let estimated = estimateCurve curves targetTime
  modelParameters estimated === modelParameters (Prelude.head curves)

-- | Property: Midpoint interpolation should yield parameters halfway between bounds.
prop_midpoint_interpolation :: Property
prop_midpoint_interpolation = property $ do
  c1 <- forAll genHDCurve
  c2 <- forAll genHDCurve
  let curves = sortOn developmentTime [c1 { developmentTime = 5 }, c2 { developmentTime = 10 }]
  let targetTime = 7.5
  let estimated = estimateCurve curves targetTime
  let p1 = modelParameters (Prelude.head curves)
  let p2 = modelParameters (curves !! 1)
  let expected = zipWith (\v1 v2 -> (v1 + v2) / 2) p1 p2
  
  -- Compare with a small epsilon
  let result = modelParameters estimated
  diff result (\rs ex -> all (\(r, e) -> abs (r - e) < 1e-6) (zip rs ex)) expected
