module Main where

import Control.Monad
import HGet.Internal.CLI (cliConfig)
import HGet.Internal.UI (runUI)

main :: IO ()
main = do
  config <- cliConfig
  void runUI
