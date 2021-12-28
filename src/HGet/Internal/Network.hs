{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HGet.Internal.Network where

import Conduit (MonadIO (liftIO), mapMC, runConduit, (.|))
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Control.Monad.Trans.Resource (runResourceT)
import qualified Control.Monad.Trans.State as ST
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Data.Conduit.Binary (sinkFile)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import GHC.Natural (intToNatural)
import Membrain (Byte, byte, showMemory, toMemory)
import Network.HTTP.Conduit
import qualified Text.Printf as T

data DownloadProgress
  = BoundedRange
      { _done :: Int,
        _total :: Int
      }
  | UnBoundedRange
      { _done :: Int
      }
  deriving (Eq)

$(makeLenses ''DownloadProgress)

instance Show DownloadProgress where
  show (BoundedRange a b) =
    let fa = fromIntegral a :: Float
        fb = fromIntegral b :: Float
     in T.printf "%.2f%%" ((100 * fa) / fb)
  show (UnBoundedRange a) = showMemory $ byte $ intToNatural a

request :: String -> FilePath -> IO ()
request url path = do
  request <- parseRequest url
  let headRequest = request {method = "HEAD"}
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http headRequest manager
    let headers = M.fromList $ responseHeaders response
    let length = fst . fromJust . B.readInt . fromJust $ M.lookup "Content-Length" headers
    let range = BoundedRange {_done = 0, _total = length}
    runResourceT $ do
      void $
        flip ST.runStateT range $ do
          response <- http request manager
          runConduit $ responseBody response .| mapMC doProgress .| sinkFile path
  where
    doProgress bs = do
      done += B.length bs
      r <- ST.get
      liftIO $ print r
      return bs