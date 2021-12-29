{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HGet.Internal.Network where

import Conduit (MonadIO (liftIO), mapMC, runConduit, runResourceT, (.|))
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Control.Monad.Trans.Resource (runResourceT)
import qualified Control.Monad.Trans.State as ST
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Data.Conduit.Binary (sinkFile)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Formatting ((%))
import qualified Formatting as F
import Network.HTTP.Conduit
import Network.HTTP.Simple (getResponseStatus, getResponseStatusCode, httpSink, setRequestHeader, setRequestMethod)
import Network.HTTP.Types
import Network.HTTP.Types.Header (hAcceptRanges, hContentLength, hContentRange)

data DownloadProgress
  = BoundProgress
      { _done :: Int,
        _total :: Int
      }
  | UnboundProgress
      { _done :: Int
      }
  deriving (Eq)

$(makeLenses ''DownloadProgress)

instance Show DownloadProgress where
  show (BoundProgress a b) =
    let fa = fromIntegral a :: Float
        fb = fromIntegral b :: Float
     in T.unpack $ F.sformat (F.float % "%") (100 * fa / fb)
  show (UnboundProgress a) = T.unpack $ F.sformat (F.bytes (F.fixed 2)) a

download :: Text -> FilePath -> IO ()
download url path = do
  request <- (parseRequest . T.unpack) url
  let headRequest = request {method = "HEAD"}
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http headRequest manager
    let headers = M.fromList $ responseHeaders response
    let length = fst . fromJust . B.readInt . fromJust $ M.lookup "Content-Length" headers
    let range = BoundProgress {_done = 0, _total = length}
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

data TaskInfo = TaskInfo
  { taskInfoUrl :: Text,
    taskInfoIsSupportRange :: Bool,
    taskInfoSizeInBytes :: Int
  }

instance Show TaskInfo where
  show (TaskInfo url supportRange sizeInBytes) =
    T.unpack $
      F.sformat
        ("{ url: " % F.stext % ", isRangeSupport: " % F.stext % ", size : " % F.bytes (F.fixed 2 % " ") % " }")
        url
        (if supportRange then "yes" else "no")
        sizeInBytes

data Task = Task
  { taskUrl :: Text,
    taskSavePath :: FilePath,
    taskSizeInBytes :: Int,
    taskSlices :: ByteRanges
  }
  deriving (Show, Eq)

data DownloadResult = RequestSuccess | RequestRangeIgnored | RequestRangeIllegal ByteRange | RequestFailed Int
  deriving (Show)

$(makeFields ''TaskInfo)
$(makeFields ''Task)

getTaskInfo :: Text -> Manager -> IO TaskInfo
getTaskInfo url manager = do
  request <- (parseRequest . T.unpack) url
  let headRequest = setRequestMethod methodHead request
  runResourceT $ do
    response <- http headRequest manager
    let headers = (M.fromList . responseHeaders) response
    let isRangeSupport = case M.lookup hAcceptRanges headers of
          (Just "bytes") -> True
          _ -> False
    let length = fst . fromJust . B.readInt . fromJust $ M.lookup hContentLength headers
    return $ TaskInfo url isRangeSupport length

downloadRange :: Request -> ByteRange -> FilePath -> Manager -> IO DownloadResult
downloadRange req range path manager = do
  let request = setRequestHeader hContentRange [renderByteRange range] req

  runResourceT $ do
    response <- http request manager
    case getResponseStatusCode response of
      200 -> return RequestRangeIgnored
      416 -> return $ RequestRangeIllegal range
      206 -> do
        void $ runConduit $ responseBody response .| sinkFile path
        return RequestSuccess
      code -> return $ RequestFailed code

splitRange :: Int -> Int -> ByteRanges
splitRange sizeInBytes segmentCnt = map go [0 .. segmentCnt - 1]
  where
    sizePerSeg :: Integer
    sizePerSeg = fromIntegral . ceiling $ (fromIntegral sizeInBytes :: Double) / (fromIntegral segmentCnt :: Double)

    go :: Int -> ByteRange
    go i = ByteRangeFromTo (i' * sizePerSeg) (s' `min` (((i' + 1) * sizePerSeg) - 1))
      where
        i' = fromIntegral i
        s' = fromIntegral sizeInBytes - 1

createTask :: Text -> Maybe FilePath -> IO Task
createTask url path = do
  manager <- newManager tlsManagerSettings
  taskInfo <- getTaskInfo url manager
  let size = taskInfo ^. sizeInBytes
  let ranges = splitRange size 4
  let savePath = case path of
        Nothing -> T.unpack $ T.takeWhileEnd ('/' /=) url
        Just s -> s
  return $ Task url savePath size ranges
