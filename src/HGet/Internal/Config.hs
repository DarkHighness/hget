module HGet.Internal.Config where

import Data.Semigroup ((<>))
import Options.Applicative

data Config = Config
  { _taskUrl :: String,
    _taskFileSaveTo :: Maybe String,
    _noUI :: Bool
  }

parseConfig :: Parser Config
parseConfig =
  Config
    <$> argument
      str
      ( metavar "URL"
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