module HGet.Internal.Config where

import Data.Semigroup ((<>))
import Data.Text (Text)
import Options.Applicative

data Config = Config
  { _taskUrl :: Maybe Text,
    _taskFileSaveTo :: Maybe Text,
    _noUI :: Bool
  }

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