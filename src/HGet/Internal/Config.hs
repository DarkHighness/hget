{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module HGet.Internal.Config where

import Control.Lens.TH ( makeFields )
import Data.Text (Text)
import Options.Applicative
    ( optional,
      argument,
      help,
      long,
      metavar,
      short,
      str,
      strOption,
      switch,
      Parser )

data Config = Config
  { configTaskUrl :: Maybe Text,
    configTaskSavePath :: Maybe Text,
    configNoUI :: Bool
  }

$(makeFields ''Config)

parseConfig :: Parser Config
parseConfig =
  Config
    <$> optional
      ( argument
          str
          ( metavar "URL"
          )
      )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "OUTPUT"
              <> help "Path to save file"
          )
      )
    <*> switch
      ( long "no-ui"
          <> help "Whether to open terminal ui"
      )