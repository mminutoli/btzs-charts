module BtzsChartsTests.Generators (
    genProcessConfig,
    genHDCurve
  ) where

import BtzsCharts.Types
import BtzsCharts.HDCurveFitting

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Vector.Storable as VS

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
