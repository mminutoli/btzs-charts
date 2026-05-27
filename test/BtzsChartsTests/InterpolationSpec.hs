{-# LANGUAGE OverloadedStrings #-}
module BtzsChartsTests.InterpolationSpec (btzsChartsInterpolationTests) where

import BtzsCharts.HDCurveFitting
import BtzsCharts.FilmAnalysis
import BtzsCharts.Interpolation
import BtzsChartsTests.Generators

import Control.Monad.Reader
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
  , testProperty "Contrast increases monotonically with development time" prop_monotonic_contrast
  , testProperty "timeForGradient finds the time for a known gradient" prop_timeForGradient_inverse
  ]

-- | Property: If target time exactly matches a measured curve, the result should be that curve.
prop_exact_match :: Property
prop_exact_match = property $ do
  c1 <- forAll genHDCurve
  c2 <- forAll genHDCurve
  -- Ensure times are distinct and sorted
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

-- | Property: If T1 < T2, then Gamma(T1) < Gamma(T2).
prop_monotonic_contrast :: Property
prop_monotonic_contrast = property $ do
  conf <- forAll genProcessConfig
  cLow <- forAll genHDCurve
  cHigh <- forAll genHDCurve
  -- Create two measured curves with distinct times and distinct slopes
  let c1 = cLow { developmentTime = 5, modelParameters = [0.1, 2.0, 1.5, 1.0] }
  let c2 = cHigh { developmentTime = 10, modelParameters = [0.1, 2.5, 3.0, 1.0] }
  let curves = [c1, c2]
  
  t1 <- forAll $ Gen.float (Range.linearFrac 6.0 7.0)
  t2 <- forAll $ Gen.float (Range.linearFrac 8.0 9.0)
  
  let curve1 = estimateCurve curves t1
  let curve2 = estimateCurve curves t2
  
  let g1 = runReader (avgGradient curve1) conf
  let g2 = runReader (avgGradient curve2) conf
  
  assert (g2 > g1)

-- | Property: If we find the time for a specific gradient, 
--   estimating a curve at that time should return that gradient.
prop_timeForGradient_inverse :: Property
prop_timeForGradient_inverse = property $ do
  conf <- forAll genProcessConfig
  cLow <- forAll genHDCurve
  cHigh <- forAll genHDCurve
  let c1 = cLow { developmentTime = 5, modelParameters = [0.1, 2.0, 1.5, 1.0] }
  let c2 = cHigh { developmentTime = 10, modelParameters = [0.1, 2.5, 3.0, 1.0] }
  let curves = [c1, c2]
  
  let g1 = runReader (avgGradient c1) conf
  let g2 = runReader (avgGradient c2) conf
  
  -- Choose a target gradient between the two
  targetG <- forAll $ Gen.double (Range.linearFrac (g1 + 0.01) (g2 - 0.01))
  
  let targetTime = runReader (timeForGradient curves targetG) conf
  let estimated = estimateCurve curves targetTime
  let actualG = runReader (avgGradient estimated) conf
  
  diff actualG (\a b -> abs (a - b) < 1e-4) targetG
