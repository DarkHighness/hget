{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HGet.Internal.Network where

import Conduit (MonadIO (..), (.|))
import qualified Conduit as C
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TChan)
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Ex
import Control.Lens (Each (each), (+=), (^.), (^..))
import Control.Lens.TH (makeFields, makeLenses)
import Control.Monad (forM_, void)
import Control.Monad.Loops (whileM_)
import qualified Control.Monad.Trans.State as ST
import Control.Retry (RetryAction (ConsultPolicy, ConsultPolicyOverrideDelay, DontRetry), RetryPolicyM)
import qualified Control.Retry as R
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Data.IORef (newIORef)
import qualified Data.IORef as IO
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Formatting ((%))
import qualified Formatting as F
import GHC.IO.Device (SeekMode (AbsoluteSeek))
import qualified GHC.IO.Handle as IO
import qualified GHC.IO.Handle.FD as IO
import GHC.IO.IOMode (IOMode (AppendMode, WriteMode))
import Network.HTTP.Conduit
  ( HttpException (..),
    Manager,
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
import Network.HTTP.Types.Header (hAcceptRanges, hRetryAfter)
import System.Directory (removeFile)

-- data DownloadProgress
--   = BoundProgress
--       { _done :: Integer,
--         _total :: Integer
--       }
--   | UnboundProgress
--       { _done :: Integer
--       }
--   deriving (Eq)

-- $(makeLenses ''DownloadProgress)

-- instance Show DownloadProgress where
--   show (BoundProgress a b) =
--     let fa = fromIntegral a :: Float
--         fb = fromIntegral b :: Float
--      in T.unpack $ F.sformat (F.float % "%") (100 * fa / fb)
--   show (UnboundProgress a) = T.unpack $ F.sformat (F.bytes (F.fixed 2)) a

-- download :: Text -> FilePath -> IO ()
-- download url path = do
--   request <- (HC.parseRequest . T.unpack) url
--   let headRequest = request {method = "HEAD"}
--   manager <- HC.newManager HC.tlsManagerSettings
--   C.runResourceT $ do
--     response <- HC.http headRequest manager
--     let headers = M.fromList $ responseHeaders response
--     let length = fst . fromJust . B.readInteger . fromJust $ M.lookup "Content-Length" headers
--     let range = BoundProgress {_done = 0, _total = length}
--     C.runResourceT $
--       void $
--         flip ST.runStateT range $ do
--           response' <- HC.http request manager
--           C.runConduit $ responseBody response' .| C.mapMC doProgress .| C.sinkFile path
--   where
--     doProgress bs = do
--       done += (fromIntegral . B.length) bs
--       r <- ST.get
--       liftIO $ print r
--       return bs

data TaskInfo = TaskInfo
  { taskInfoUrl :: Text,
    taskInfoIsSupportRange :: Bool,
    taskInfoSizeInBytes :: Integer
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
    taskRangeByteRange :: WorkerRange,
    taskRangeIndex :: Integer
  }
  deriving (Show, Eq)

type TaskRanges = [TaskRange]

data Task = Task
  { taskUrl :: Text,
    taskRequest :: Request,
    taskManager :: Manager,
    taskSavePath :: FilePath,
    taskSizeInBytes :: Integer,
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

data DownloadResult = RequestSuccess | RequestRangeIgnored | RequestRangeIllegal ByteRange | RequestTooManyRequest (Maybe Int) | RequestFailed Int
  deriving (Show)

data WorkerEvent = WorkerProgressUpdateEvent {_weWhich :: Int, _weFrom :: Integer, _weNow :: Integer, _weTo :: Integer} | WorkDoneEvent {_weWhich :: Int} | WorkFailedEvent {_weWhich :: Int}
  deriving (Show)

data WorkerRange = WorkerRange {_wrFrom :: Integer, _wrTo :: Integer}
  deriving (Show, Eq)

toByteRange :: WorkerRange -> ByteRange
toByteRange (WorkerRange a b) = ByteRangeFromTo a b

$(makeFields ''TaskInfo)
$(makeFields ''TaskRange)
$(makeFields ''Task)
$(makeFields ''WorkerEvent)
$(makeFields ''WorkerRange)

getTaskInfo :: Text -> Request -> Manager -> IO TaskInfo
getTaskInfo url req manager = do
  let headRequest = HC.setRequestMethod methodHead req
  C.runResourceT $ do
    response <- HC.http headRequest manager
    let headers = (M.fromList . responseHeaders) response
    let isRangeSupport = case M.lookup hAcceptRanges headers of
          (Just "bytes") -> True
          _ -> False
    let length = fst . fromJust . B.readInteger . fromJust $ M.lookup hContentLength headers
    return $ TaskInfo url isRangeSupport length

downloadRangeRetry :: Int -> Request -> WorkerRange -> FilePath -> Manager -> TChan WorkerEvent -> RetryPolicyM IO -> IO ()
downloadRangeRetry workId req range path manager chan policy = void $ do
  ref <- newIORef initialState

  R.retryingDynamic
    policy
    ( \_ r -> do
        return
          ( case r of
              RequestSuccess -> DontRetry
              RequestRangeIgnored -> DontRetry
              RequestRangeIllegal _ -> DontRetry
              RequestTooManyRequest Nothing -> ConsultPolicy
              RequestTooManyRequest (Just delay) -> ConsultPolicyOverrideDelay (1000 * delay)
              RequestFailed _ -> ConsultPolicy
          )
    )
    ( \_ -> do
        r <- (Ex.try :: IO DownloadResult -> IO (Either HttpException DownloadResult)) $ do
          p' <- IO.readIORef ref

          let req' = HC.setRequestHeader hRange [HC.renderByteRanges [createRange (toByteRange range) p']] req

          C.runResourceT $ do
            resp <- HC.http req' manager
            case HC.getResponseStatusCode resp of
              200 -> return RequestRangeIgnored
              416 -> return $ RequestRangeIllegal range
              429 -> do
                case HC.getResponseHeader hRetryAfter resp of
                  [] -> return $ RequestTooManyRequest Nothing
                  (h : _) -> (return . RequestTooManyRequest . Just . fst . fromJust . B.readInt) h
              206 -> do
                handle <- liftIO $ do
                  handle <- IO.openBinaryFile path WriteMode
                  IO.hSeek handle AbsoluteSeek p'
                  return handle

                C.runConduit $ do
                  do
                    responseBody resp
                    .| C.mapMC
                      ( \bs -> do
                          liftIO $ do
                            t <- IO.readIORef ref
                            let n = B.length bs
                            let t' = t + fromIntegral n
                            IO.atomicWriteIORef ref t'
                            STM.atomically $ do
                              STM.writeTChan chan $ WorkerProgressUpdateEvent workId
                          return bs
                      )
                    .| C.sinkHandle handle

                liftIO $ IO.hClose handle

                return RequestSuccess
              code -> return $ RequestFailed code
        case r of
          Left e -> do
            print e
            return $ RequestFailed (-1)
          Right r -> return r
    )
  where
    initialState :: Integer
    initialState = case range of
      ByteRangeFrom n -> n
      ByteRangeFromTo n _ -> n
      ByteRangeSuffix _ -> error "unspport range type"

    createRange :: ByteRange -> Integer -> ByteRange
    createRange (ByteRangeFrom _) n = ByteRangeFrom n
    createRange (ByteRangeFromTo _ b) n = ByteRangeFromTo n b
    createRange _ _ = error "unsupport range type"

-- downloadRange :: Request -> ByteRange -> FilePath -> Manager -> TChan Int -> IO DownloadResult
-- downloadRange req range path manager chan = do
--   let request = HC.setRequestHeader hRange [HC.renderByteRanges [range]] req
--   C.runResourceT $ do
--     response <- HC.http request manager
--     case HC.getResponseStatusCode response of
--       200 -> return RequestRangeIgnored
--       416 -> return $ RequestRangeIllegal range
--       206 -> do
--         void $ C.runConduit $ responseBody response .| C.mapMC progress .| C.sinkFile path
--         return RequestSuccess
--       code -> return $ RequestFailed code
--   where
--     progress bs = do
--       liftIO . STM.atomically $
--         STM.writeTChan chan (B.length bs)
--       return bs

splitRange :: Integer -> Integer -> ByteRanges
splitRange sizeInBytes segmentCnt = map go [(0 :: Integer) .. (pred segmentCnt)]
  where
    sizePerSeg :: Integer
    sizePerSeg = fromIntegral . ceiling $ (fromIntegral sizeInBytes :: Double) / (fromIntegral segmentCnt :: Double)

    go :: Integer -> ByteRange
    go i = ByteRangeFromTo (i' * sizePerSeg) (s' `min` ((i' + 1) * sizePerSeg - 1))
      where
        i' = fromIntegral i
        s' = fromIntegral sizeInBytes - 1

byteRangeToTaskRange :: FilePath -> Integer -> Integer -> ByteRange -> TaskRange
byteRangeToTaskRange basePath 1 rangeIdx range = TaskRange basePath range rangeIdx
byteRangeToTaskRange basePath totalCnt rangeIdx range | totalCnt > 1 = TaskRange (basePath ++ ".part" ++ show rangeIdx) range rangeIdx
byteRangeToTaskRange _ _ _ _ = error "illegal count"

createTask :: Text -> FilePath -> Integer -> IO Task
createTask url path threadCnt = do
  manager <- HC.newManager HC.tlsManagerSettings
  request <- HC.parseRequest . T.unpack $ url
  taskInfo <- getTaskInfo url request manager
  let size = taskInfo ^. sizeInBytes
  let ranges = zipWith (byteRangeToTaskRange path threadCnt) [1 ..] (splitRange size threadCnt)
  return $ Task url request manager path size ranges

mergeFiles :: FilePath -> [FilePath] -> IO ()
mergeFiles path paths =
  C.runConduitRes $
    C.yieldMany paths
      .| C.awaitForever C.sourceFile
      .| C.sinkFile path

doTask :: Task -> IO ()
doTask (Task _ request manager path size ranges) = void $
  flip ST.evalStateT (0 :: Integer) $ do
    chan <- liftIO STM.newTChanIO

    void $
      liftIO $ do
        forM_
          (zip ranges [1 ..])
          ( \(workerId, TaskRange tempPath range _) ->
              forkIO $ do
                downloadRangeRetry workerId request range tempPath manager chan R.retryPolicyDefault
          )

    whileM_
      ( do
          c <- ST.get
          return $ c < size
      )
      ( do
          c <- liftIO $
            STM.atomically $ do
              STM.readTChan chan
          s <- ST.get
          let s' = s + c
          ST.put s'
      )

    void $
      liftIO $ do
        let files = ranges ^.. each . filePath
        case length files of
          0 -> error "illegal temp files count"
          1 -> return ()
          _ -> do
            mergeFiles path files
            forM_ files removeFile

download' :: Text -> FilePath -> Integer -> IO ()
download' url path threadCount = do
  task <- createTask url path threadCount
  void $ doTask task
