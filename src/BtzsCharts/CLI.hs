{-|
Module      : BtzsCharts.CLI
Description : Command line interface for btzs-charts.
Copyright   : (c) Marco Minutoli, 2025

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX
-}
module BtzsCharts.CLI (
    BtzsOptions(..),
    parseBtzsOptions,
    optsParser
  ) where

import Options.Applicative

data BtzsOptions = BtzsOptions
  { optConfig :: FilePath
  , optFilm   :: Maybe FilePath
  , optPaper  :: Maybe FilePath
  , optStepTablet :: FilePath
  } deriving (Show, Eq)

parseBtzsOptions :: IO BtzsOptions
parseBtzsOptions = execParser btzsInfo

btzsInfo :: ParserInfo BtzsOptions
btzsInfo = info (optsParser <**> helper)
  ( fullDesc
  <> progDesc "Generate characteristic curves and analysis for BTZS"
  <> header "btzs-charts - Photographic material analysis tool" )

optsParser :: Parser BtzsOptions
optsParser = BtzsOptions
  <$> strOption
      ( long "config"
     <> short 'c'
     <> metavar "CONFIG_JSON"
     <> help "Path to the process configuration JSON file"
     <> value "./data/ProcessConfig.json"
     <> showDefault )
  <*> optional (strOption
      ( long "film"
     <> short 'f'
     <> metavar "FILM_JSON"
     <> help "Path to the film test results JSON file" ))
  <*> optional (strOption
      ( long "paper"
     <> short 'p'
     <> metavar "PAPER_JSON"
     <> help "Path to the paper test results JSON file" ))
  <*> strOption
      ( long "step-tablet"
     <> short 's'
     <> metavar "STEP_TABLET_JSON"
     <> help "Path to the step tablet definition JSON file"
     <> value "./data/Stauffer-21steps.json"
     <> showDefault )
