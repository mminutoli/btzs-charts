{-# LANGUAGE OverloadedStrings #-}
module BtzsChartsTests.FieldChartsSpec (btzsChartsFieldChartsTests) where

import BtzsCharts.FieldCharts
import BtzsCharts.HDCurveFitting (HDCurve(..))
import BtzsChartsTests.Generators

import Control.Monad (forM_)
import Control.Monad.Reader
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.Hedgehog ( testProperty )
import Data.List (sortOn)

btzsChartsFieldChartsTests :: TestTree
btzsChartsFieldChartsTests = testGroup "Tests for BtzsCharts.FieldCharts"
  [ testProperty "ISO speed increases with development time" prop_speed_increases_with_devtime
  , testProperty "N-value decreases as average gradient increases" prop_nvalue_decreases_with_gradient
  , testProperty "N-value and ISO speed calculations match standard formulas" prop_calculations_match_formulas
  , testProperty "Analytical models approximate measured curve stats points" prop_models_approximate_stats
  ]

-- Helper to create a test film curves family
testFilmCurves :: PropertyT IO [HDCurve]
testFilmCurves = do
  cLow <- forAll genHDCurve
  -- We set up 3 curves representing a range of development times (e.g. 5, 8, 12 minutes)
  -- with corresponding gradients and speed points
  let c1 = cLow  { developmentTime = 5.0,  modelParameters = [0.1, 2.0, 1.5, 2.0] }
      c2 = cLow  { developmentTime = 8.0,  modelParameters = [0.1, 2.2, 2.0, 1.0] }
      c3 = cLow  { developmentTime = 12.0, modelParameters = [0.1, 2.4, 2.5, 0.0] }
  return [c1, c2, c3]

-- | Property: ISO speed must strictly increase with development time.
prop_speed_increases_with_devtime :: Property
prop_speed_increases_with_devtime = property $ do
  conf <- forAll genProcessConfig
  curves <- testFilmCurves
  sbr <- forAll $ Gen.double (Range.linearFrac 4.0 12.0)
  ler <- forAll $ Gen.double (Range.linearFrac 0.6 1.8)
  ratedIso <- forAll $ Gen.double (Range.linearFrac 50.0 400.0)

  let stats = runReader (calculateCurveStats curves ratedIso ler sbr) conf
      sorted = sortOn statsDevTime stats
      speeds = map statsIsoSpeed sorted

  -- S_1 < S_2 < S_3 ...
  assert (all (\(s1, s2) -> s1 < s2) (zip speeds (drop 1 speeds)))

-- | Property: N-value must strictly decrease as average gradient increases.
prop_nvalue_decreases_with_gradient :: Property
prop_nvalue_decreases_with_gradient = property $ do
  conf <- forAll genProcessConfig
  curves <- testFilmCurves
  sbr <- forAll $ Gen.double (Range.linearFrac 4.0 12.0)
  ler <- forAll $ Gen.double (Range.linearFrac 0.6 1.8)
  ratedIso <- forAll $ Gen.double (Range.linearFrac 50.0 400.0)

  let stats = runReader (calculateCurveStats curves ratedIso ler sbr) conf
      sorted = sortOn statsGradient stats
      nvals = map statsNValue sorted

  -- N_1 > N_2 > N_3 ... (larger gradient means more negative N-value)
  assert (all (\(n1, n2) -> n1 > n2) (zip nvals (drop 1 nvals)))

-- | Property: Film stats matches exact mathematical formulas.
prop_calculations_match_formulas :: Property
prop_calculations_match_formulas = property $ do
  conf <- forAll genProcessConfig
  curves <- testFilmCurves
  sbr <- forAll $ Gen.double (Range.linearFrac 4.0 12.0)
  ler <- forAll $ Gen.double (Range.linearFrac 0.6 1.8)
  ratedIso <- forAll $ Gen.double (Range.linearFrac 50.0 400.0)

  let stats = runReader (calculateCurveStats curves ratedIso ler sbr) conf

  -- Reference speed point E_ref calculated on standardAvgGradient curve (0.58)
  forM_ stats $ \entry -> do
    -- Verify N-value matches formula: N = LER / (0.3 * G) - SBR
    let expectedN = ler / (0.3 * statsGradient entry) - sbr
    diff (statsNValue entry) (\a b -> abs (a - b) < 1e-6) expectedN

    -- Verify relative ISO speed ratios: S_i / S_j = 2^((e_j - e_i) / 0.3)
    let speedsWithExposure = map (\e -> (statsIsoSpeed e, statsSpeedPoint e)) stats
    forM_ (zip speedsWithExposure (drop 1 speedsWithExposure)) $ \((s1, e1), (s2, e2)) -> do
      let expectedRatio = 2.0 ** ((e2 - e1) / 0.3)
          actualRatio = s1 / s2
      diff actualRatio (\a b -> abs (a - b) < 1e-6) expectedRatio

-- | Property: Analytical fitted models approximate measured stats points within regression tolerances.
prop_models_approximate_stats :: Property
prop_models_approximate_stats = property $ do
  conf <- forAll genProcessConfig
  curves <- testFilmCurves
  sbr <- forAll $ Gen.double (Range.linearFrac 5.0 9.0)
  ler <- forAll $ Gen.double (Range.linearFrac 0.8 1.4)
  ratedIso <- forAll $ Gen.double (Range.linearFrac 100.0 200.0)

  let stats = runReader (calculateCurveStats curves ratedIso ler sbr) conf
      model = fitFieldChartModel stats

  -- Evaluating fitted models at measured N-values should yield values close to measured times/speeds
  forM_ stats $ \entry -> do
    let predTime = evaluateTimeModel model (statsNValue entry)
        predSpeed = evaluateSpeedModel model (statsNValue entry)

    -- Development time within 2 minutes of measured
    diff predTime (\a b -> abs (a - b) < 2.0) (statsDevTime entry)
    -- ISO Speed within 20% of measured
    diff predSpeed (\a b -> abs (a - b) / b < 0.20) (statsIsoSpeed entry)
