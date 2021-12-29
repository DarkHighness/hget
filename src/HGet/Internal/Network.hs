{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HGet.Internal.Network where

import Conduit (MonadIO (..), (.|))
import qualified Conduit as C
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TChan)
import qualified Control.Concurrent.STM as STM
import Control.Lens ((+=), (^.))
import Control.Lens.TH (makeFields, makeLenses)
import Control.Monad (forM_, void)
import Control.Monad.Loops (untilM)
import qualified Control.Monad.Trans.State as ST
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Data.IORef (atomicWriteIORef, newIORef, readIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Formatting ((%))
import qualified Formatting as F
import Network.HTTP.Conduit
  ( Manager,
    Request (..),
    Response (..),
  )
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Simple as HC
import Network.HTTP.Types
  ( ByteRange (..),
    ByteRanges,
    hContentLength,
    hRange,
    methodHead,
  )
import qualified Network.HTTP.Types as HC
import Network.HTTP.Types.Header (hAcceptRanges)

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
  request <- (HC.parseRequest . T.unpack) url
  let headRequest = request {method = "HEAD"}
  manager <- HC.newManager HC.tlsManagerSettings
  C.runResourceT $ do
    response <- HC.http headRequest manager
    let headers = M.fromList $ responseHeaders response
    let length = fst . fromJust . B.readInt . fromJust $ M.lookup "Content-Length" headers
    let range = BoundProgress {_done = 0, _total = length}
    C.runResourceT $
      void $
        flip ST.runStateT range $ do
          response' <- HC.http request manager
          C.runConduit $ responseBody response' .| C.mapMC doProgress .| C.sinkFile path
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
        ("{ url: " % F.stext % ", isRangeSupport: " % F.stext % ", size : " % F.bytes (F.fixed 2) % " }")
        url
        (if supportRange then "yes" else "no")
        sizeInBytes

data TaskRange = TaskRange
  { taskRangeFilePath :: FilePath,
    taskRangeByteRange :: ByteRange,
    taskRangeIndex :: Int
  }
  deriving (Show, Eq)

type TaskRanges = [TaskRange]

data Task = Task
  { taskUrl :: Text,
    taskRequest :: Request,
    taskManager :: Manager,
    taskSavePath :: FilePath,
    taskSizeInBytes :: Int,
    taskSlices :: TaskRanges
  }

instance Show Task where
  show (Task url req _ path size slices) =
    T.unpack $
      F.sformat
        ("{ url: " % F.stext % ", request: " % F.string % ", path: " % F.string % ", size: " % F.bytes (F.fixed 2) % ", slices: " % F.string % "}")
        url
        (show req)
        path
        size
        (show slices)

data DownloadResult = RequestSuccess | RequestRangeIgnored | RequestRangeIllegal ByteRange | RequestFailed Int
  deriving (Show)

$(makeFields ''TaskInfo)
$(makeFields ''TaskRange)
$(makeFields ''Task)

getTaskInfo :: Text -> Request -> Manager -> IO TaskInfo
getTaskInfo url req manager = do
  let headRequest = HC.setRequestMethod methodHead req
  C.runResourceT $ do
    response <- HC.http headRequest manager
    let headers = (M.fromList . responseHeaders) response
    let isRangeSupport = case M.lookup hAcceptRanges headers of
          (Just "bytes") -> True
          _ -> False
    let length = fst . fromJust . B.readInt . fromJust $ M.lookup hContentLength headers
    return $ TaskInfo url isRangeSupport length

downloadRange :: Request -> ByteRange -> FilePath -> Manager -> TChan Int -> IO DownloadResult
downloadRange req range path manager chan = do
  let request = HC.setRequestHeader hRange [HC.renderByteRanges [range]] req
  C.runResourceT $ do
    response <- HC.http request manager
    case HC.getResponseStatusCode response of
      200 -> return RequestRangeIgnored
      416 -> return $ RequestRangeIllegal range
      206 -> do
        void $ C.runConduit $ responseBody response .| C.mapMC progress .| C.sinkFile path
        return RequestSuccess
      code -> return $ RequestFailed code
  where
    progress bs = do
      liftIO . STM.atomically $
        STM.writeTChan chan (B.length bs)
      return bs

splitRange :: Int -> Int -> ByteRanges
splitRange sizeInBytes segmentCnt = map go [0 .. segmentCnt - 1]
  where
    sizePerSeg :: Integer
    sizePerSeg = fromIntegral . ceiling $ (fromIntegral sizeInBytes :: Double) / (fromIntegral segmentCnt :: Double)

    go :: Int -> ByteRange
    go i = ByteRangeFromTo (i' * sizePerSeg) (s' `min` ((i' + 1) * sizePerSeg - 1))
      where
        i' = fromIntegral i
        s' = fromIntegral sizeInBytes - 1

byteRangeToTaskRange :: FilePath -> Int -> Int -> ByteRange -> TaskRange
byteRangeToTaskRange basePath 1 rangeIdx range = TaskRange basePath range rangeIdx
byteRangeToTaskRange basePath totalCnt rangeIdx range | totalCnt > 1 = TaskRange (basePath ++ ".part" ++ show rangeIdx) range rangeIdx
byteRangeToTaskRange _ _ _ _ = error "illegal count"

createTask :: Text -> FilePath -> Int -> IO Task
createTask url path threadCnt = do
  manager <- HC.newManager HC.tlsManagerSettings
  request <- HC.parseRequest . T.unpack $ url
  taskInfo <- getTaskInfo url request manager
  let size = taskInfo ^. sizeInBytes
  let ranges = zipWith (byteRangeToTaskRange path threadCnt) [1 ..] (splitRange size threadCnt)
  return $ Task url request manager path size ranges

mergeFiles :: FilePath -> [FilePath] -> IO ()
mergeFiles path paths = do
  C.runConduitRes $
    C.yieldMany paths
      .| C.awaitForever C.sourceFile
      .| C.sinkFile path

doTask :: Task -> IO ()
doTask (Task _ request manager path size ranges) = do
  chan <- STM.newTChanIO
  forM_ ranges $ \(TaskRange path range _) ->
    forkIO $ do
      r <- downloadRange request range path manager chan
      case r of
        RequestSuccess -> return ()
        RequestRangeIgnored -> error "ignored"
        RequestRangeIllegal br -> error $ "illegal range" ++ show br
        RequestFailed n -> error . show $ n
  count <- newIORef 0
  void $ untilM (progress count chan) (predicate count size)
  mergeFiles path (map taskRangeFilePath ranges)
  where
    progress ref chan = do
      bytes <- STM.atomically $ do
        STM.readTChan chan
      prev <- readIORef ref
      let new = prev + bytes
      print new
      atomicWriteIORef ref new

    predicate ref hope = do
      count <- readIORef ref
      return $ count >= hope

download' :: Text -> FilePath -> Int -> IO ()
download' url path threadCount = do
  task <- createTask url path threadCount
  void $ doTask task
