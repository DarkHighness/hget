module HGet.Internal.Settings where

import Data.Semigroup ((<>))
import Options.Applicative

data Settings = Settings
  { _taskUrl :: String,
    _taskFileSaveTo :: Maybe String,
    _noUI :: Bool
  }

parseSettings :: Parser Settings
parseSettings =
  Settings
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