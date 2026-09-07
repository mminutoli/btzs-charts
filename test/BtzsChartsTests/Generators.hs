{-# LANGUAGE OverloadedStrings #-}
module BtzsChartsTests.Generators (
    genProcessConfig,
    genZoneSystemConfig,
    genPaperProcessConfig,
    genFilmProcessConfig,
    genHDCurve
  ) where

import BtzsCharts.Types
import BtzsCharts.HDCurveFitting

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Vector.Storable as VS

-- | Generator for ZoneSystemConfig.
genZoneSystemConfig :: Gen ZoneSystemConfig
genZoneSystemConfig = do
  nz <- Gen.element [10, 11]
  ori <- Gen.element [BlackIsZone0, WhiteIsZone0]
  let (hl, sh) = case ori of
        BlackIsZone0 -> if nz == 10 then (9.0, 2.0) else (8.0, 2.0)
        WhiteIsZone0 -> if nz == 10 then (0.0, 7.0) else (1.0, 7.0)
  return $ ZoneSystemConfig nz ori hl sh mempty

-- | Generator for PaperProcessConfig.
genPaperProcessConfig :: Gen PaperProcessConfig
genPaperProcessConfig = do
  grade <- Gen.element ["00", "0", "1", "2", "3", "4", "5"]
  mHl <- Gen.maybe (Gen.double (Range.linearFrac 0.02 0.10))
  mSh <- Gen.maybe (Gen.double (Range.linearFrac 0.70 0.95))
  return $ PaperProcessConfig grade mHl mSh

-- | Generator for FilmProcessConfig.
genFilmProcessConfig :: Gen FilmProcessConfig
genFilmProcessConfig = do
  spd <- Gen.double (Range.linearFrac 0.05 0.2)
  sag <- Gen.double (Range.linearFrac 0.4 1.2)
  spf <- Gen.double (Range.linearFrac 0.5 1.5)
  fcf <- Gen.double (Range.linearFrac 0.5 1.5)
  sbr <- Gen.maybe (Gen.double (Range.linearFrac 5.0 9.0))
  return $ FilmProcessConfig spd sag spf fcf sbr

-- | Generator for ProcessConfiguration.
genProcessConfig :: Gen ProcessConfiguration
genProcessConfig = Gen.filter (\c -> (standardAvgGradient c / speedPointFactor c) * flareCompensationFactor c < 1.7) rawGen
  where
    rawGen = do
      sag <- Gen.double (Range.linearFrac 0.4 1.2)
      spf <- Gen.double (Range.linearFrac 0.5 1.5)
      fcf <- Gen.double (Range.linearFrac 0.5 1.5)
      zr  <- Gen.double (Range.linearFrac 5.0 9.0)
      spd <- Gen.double (Range.linearFrac 0.05 0.2)
      pspd <- Gen.double (Range.linearFrac 0.02 0.06)
      pidm <- Gen.double (Range.linearFrac 0.85 0.95)
      let zs = ZoneSystemConfig 10 BlackIsZone0 9.0 2.0 mempty
          pap = PaperProcessConfig "2" Nothing (Just pidm)
          flm = FilmProcessConfig spd sag spf fcf (Just zr)
      return $ ProcessConfiguration "2.0" "Generated Process" zs pap flm (Just pspd)


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
