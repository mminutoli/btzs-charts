{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : BtzsChartsTests.HDCurveFittingSpec
Description : Tests for the module BtzsCharts.HDCurveFitting.
Copyright   : (c) Marco Minutoli, 2025

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX
-}

module BtzsChartsTests.HDCurveFittingSpec (btzsChartsHDCurveFittingTests) where

import BtzsCharts.Types
import BtzsCharts.HDCurveFitting
import BtzsChartsTests.Generators

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.Hedgehog ( testProperty )

btzsChartsHDCurveFittingTests :: TestTree
btzsChartsHDCurveFittingTests = testGroup "Tests for BtzsCharts.HDCurveFitting"
  [ testProperty "exposureForDensity is the inverse of the model" prop_exposure_inverse
  , testProperty "basePlusFog returns the dMin parameter" prop_basePlusFog_is_dMin
  ]

-- | Property: exposureForDensity accurately inverts the logistic model.
prop_exposure_inverse :: Property
prop_exposure_inverse = property $ do
  curve <- forAll genHDCurve
  let [dMin, dMax, _, _] = modelParameters curve
  -- Choose a target density within the sigmoid's active range
  targetD <- forAll $ Gen.double (Range.linearFrac (dMin + 0.05) (dMax - 0.05))
  let e = exposureForDensity curve targetD
  let [calcD] = logisticModel (modelParameters curve) e
  diff calcD (\a b -> abs (a - b) < 1e-4) targetD

-- | Property: basePlusFog should return the first parameter (dMin) of the model.
prop_basePlusFog_is_dMin :: Property
prop_basePlusFog_is_dMin = property $ do
  curve <- forAll genHDCurve
  let [dMin, _, _, _] = modelParameters curve
  basePlusFog curve === dMin
