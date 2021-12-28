module HGet.Internal.Network where

import Conduit (MonadIO (liftIO), mapMC, runConduit, (.|))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy as B
import Data.Conduit.Binary (sinkFile)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Network.HTTP.Conduit

request :: String -> FilePath -> IO ()
request url path = do
  request <- parseRequest url
  let headRequest = request {method = "HEAD"}
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http headRequest manager
    let headers = M.fromList $ responseHeaders response
    let length = fst . fromJust . B.readInteger . fromJust $ M.lookup "Content-Length" headers
    liftIO $ print length
    runResourceT $ do
      response <- http request manager
      runConduit $ responseBody response .| progress .| sinkFile path
  where
    progress = mapMC (\x -> do (liftIO . print) x >> return x)