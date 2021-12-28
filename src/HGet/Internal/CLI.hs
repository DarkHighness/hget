module HGet.Internal.CLI where

import HGet.Internal.Config (Config)
import qualified HGet.Internal.Config as C
import Options.Applicative (Parser, ParserInfo)
import qualified Options.Applicative as CLI
import Options.Applicative.Types (ParseError (..))
import qualified Options.Applicative.Types as CLI

version :: Parser (a -> a)
version =
  CLI.option versionReader $
    mconcat
      [ CLI.long "version",
        CLI.short 'v',
        CLI.help "Show hget version",
        CLI.value id,
        CLI.metavar "",
        CLI.noArgError
          (InfoMsg "HGet 0.0.0.1"),
        CLI.hidden
      ]
  where
    versionReader = do
      pc <- CLI.readerAsk
      CLI.readerAbort $
        ShowHelpText (Just pc)

cliConfig :: IO Config
cliConfig = do
  CLI.execParser opts
  where
    opts :: ParserInfo Config
    opts =
      CLI.info
        (C.parseConfig CLI.<**> version CLI.<**> CLI.helper)
        (CLI.fullDesc <> CLI.progDesc "A simple `get` program written in Haskell." <> CLI.header "HGet 0.0.0.1")
