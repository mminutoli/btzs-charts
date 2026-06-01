{-# LANGUAGE OverloadedStrings #-}
module BtzsChartsTests.CLISpec (btzsChartsCLITests) where

import Test.Tasty
import Test.Tasty.HUnit
import Options.Applicative
import BtzsCharts.CLI

btzsChartsCLITests :: TestTree
btzsChartsCLITests = testGroup "CLI Parser Tests"
  [ testCase "Parse default options" $ do
      let result = execParserPure defaultPrefs (info (optsParser <**> helper) fullDesc) []
      case result of
        Success opts -> do
          optConfig opts @?= "./data/ProcessConfig.json"
          optStepTablet opts @?= "./data/Stauffer-21steps.json"
          optFilm opts @?= Nothing
          optPaper opts @?= Nothing
          optSbrs opts @?= []
          optGrade opts @?= "2"
        _ -> assertFailure "Failed to parse default options"

  , testCase "Parse explicit film and paper" $ do
      let args = ["-f", "myfilm.json", "--paper", "mypaper.json", "-c", "myconfig.json"]
          result = execParserPure defaultPrefs (info (optsParser <**> helper) fullDesc) args
      case result of
        Success opts -> do
          optConfig opts @?= "myconfig.json"
          optFilm opts @?= Just "myfilm.json"
          optPaper opts @?= Just "mypaper.json"
          optSbrs opts @?= []
          optGrade opts @?= "2"
        _ -> assertFailure "Failed to parse explicit film and paper options"

  , testCase "Parse multiple SBR values and custom grade" $ do
      let args = ["--sbr", "5.0", "--sbr", "7.0", "--sbr", "9.0", "-g", "00"]
          result = execParserPure defaultPrefs (info (optsParser <**> helper) fullDesc) args
      case result of
        Success opts -> do
          optSbrs opts @?= [5.0, 7.0, 9.0]
          optGrade opts @?= "00"
        _ -> assertFailure "Failed to parse multiple SBR values and custom grade"
  ]
