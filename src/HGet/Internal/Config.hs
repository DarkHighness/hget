{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module HGet.Internal.Config where

import Control.Lens.TH
import Data.Semigroup ((<>))
import Data.Text (Text)
import Options.Applicative

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