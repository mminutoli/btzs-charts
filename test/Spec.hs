{-|
Module      : Main
Description : Tests for the module BtzsCharts package.
Copyright   : (c) Marco Minutoli, 2025

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX

This module tests the properties and functionalities of the BtzsCharts package.
-}
module Main (main) where

import Test.Tasty

import BtzsChartsTests.TypesSpec(btzsChartsTypesTests)

tests :: TestTree
tests = testGroup "Tests" [btzsChartsTypesTests]

main :: IO ()
main = defaultMain tests
