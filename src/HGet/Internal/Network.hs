{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HGet.Internal.Network where

import Conduit (MonadIO (..), (.|))
import qualified Conduit as C
import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.STM (TChan)
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Ex
import Control.Lens (Each (each), ix, sumOf, (&), (.~), (^.), (^..))
import Control.Monad (forM_, void)
import Control.Monad.Loops (untilM_, whileM_)
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
import GHC.IO.IOMode (IOMode (WriteMode))
import HGet.Internal.TH (makeSimple)
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
    hContentLength,
    hRange,
    methodHead,
  )
import qualified Network.HTTP.Types as HC
import Network.HTTP.Types.Header (hAcceptRanges, hRetryAfter)
import System.Directory (removeFile)

data TaskInfo = TaskInfo
  { _tiUrl :: Text,
    _tiSupportRange :: Bool,
    _tiTotalInBytes :: Integer
  }

instance Show TaskInfo where
  show (TaskInfo url supportRange sizeInBytes) =
    T.unpack $
      F.sformat
        ("{ url: " % F.stext % ", isRangeSupport: " % F.stext % ", size : " % F.bytes (F.fixed 2) % " }")
        url
        (if supportRange then "yes" else "no")
        sizeInBytes

data Range = Range {rangeFrom :: Integer, rangeTo :: Integer}
  deriving (Show, Eq)

$(makeSimple ''Range)

type Ranges = [Range]

toByteRange :: Range -> ByteRange
toByteRange (Range a b) = ByteRangeFromTo a b

data TaskRange = TaskRange
  { _trSavePath :: FilePath,
    _trRange :: Range,
    _trRangeIndex :: Integer
  }
  deriving (Show, Eq)

type TaskRanges = [TaskRange]

data Task = Task
  { _tUrl :: Text,
    _tRequest :: Request,
    _tManager :: Manager,
    _tSavePath :: FilePath,
    _tTotalInBytes :: Integer,
    _tSlices :: TaskRanges
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

data WorkerStatus = Ready | Running | Failed | Done
  deriving (Show, Eq, Ord)

data TaskRunningWorkerState = TaskRunningWorkerState
  { _trwsWhich :: Int,
    _trwsNow :: Integer,
    _trwsTotal :: Integer,
    _trwsStatus :: WorkerStatus
  }
  deriving (Show)

data TaskRunningState = TaskRunningState
  { _trsNow :: Integer,
    _trsTotal :: Integer,
    _trsWorkers :: Int,
    _trsWorkerStates :: [TaskRunningWorkerState]
  }
  deriving (Show)

$(makeSimple ''TaskRunningState)
$(makeSimple ''TaskRunningWorkerState)

data RequestResult
  = RequestSuccess
  | RequestRangeIgnored
  | RequestRangeIllegal Range
  | RequestTooManyRequest (Maybe Int)
  | RequestFailed Int
  deriving (Show)

data WorkerEvent
  = WorkerStartRunningEvent {_weWhich :: Int}
  | WorkerProgressUpdateEvent {_weWhich :: Int, _weWorkerRange :: Range, _weWorkerNow :: Integer}
  | WorkerDoneEvent {_weWhich :: Int, _weResult :: RequestResult}
  | WorkerFailedEvent {_weWhich :: Int, _weEx :: HttpException}
  deriving (Show)

$(makeSimple ''TaskInfo)
$(makeSimple ''TaskRange)
$(makeSimple ''Task)
$(makeSimple ''WorkerEvent)

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

downloadRangeRetry :: Int -> Request -> Range -> FilePath -> Manager -> TChan WorkerEvent -> RetryPolicyM IO -> IO ()
downloadRangeRetry workId req range path manager chan policy = void $ do
  ref <- newIORef 0

  R.retryingDynamic
    policy
    ( \_ r ->
        return
          ( case r of
              Left _ -> DontRetry
              Right r -> case r of
                RequestSuccess -> DontRetry
                RequestRangeIgnored -> DontRetry
                RequestRangeIllegal _ -> DontRetry
                RequestTooManyRequest Nothing -> ConsultPolicy
                RequestTooManyRequest (Just delay) -> ConsultPolicyOverrideDelay (1000 * delay)
                RequestFailed _ -> ConsultPolicy
          )
    )
    ( \_ -> do
        r <- (Ex.try :: IO RequestResult -> IO (Either HttpException RequestResult)) $ do
          p' <- IO.readIORef ref

          let req' = HC.setRequestHeader hRange [HC.renderByteRanges [toByteRange $ from .~ (range ^. from + p') $ range]] req

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

                C.runConduit $
                  responseBody resp
                    .| C.mapMC
                      ( \bs -> do
                          liftIO $ do
                            t <- IO.readIORef ref
                            let n = B.length bs
                            let t' = t + fromIntegral n
                            IO.atomicWriteIORef ref t'
                            STM.atomically $
                              STM.writeTChan chan (WorkerProgressUpdateEvent workId range t')
                          return bs
                      )
                    .| C.sinkHandle handle

                liftIO $ IO.hClose handle

                return RequestSuccess
              code -> return $ RequestFailed code
        liftIO $
          STM.atomically $
            STM.writeTChan chan $ case r of
              Left e -> WorkerFailedEvent workId e
              Right r -> WorkerDoneEvent workId r
        return r
    )

splitRange :: Integer -> Integer -> Ranges
splitRange sizeInBytes segmentCnt = map go [(0 :: Integer) .. (pred segmentCnt)]
  where
    sizePerSeg :: Integer
    sizePerSeg = ceiling $ (fromIntegral sizeInBytes :: Double) / (fromIntegral segmentCnt :: Double)

    go :: Integer -> Range
    go i = Range (i * sizePerSeg) (s' `min` ((i + 1) * sizePerSeg - 1))
      where
        s' = sizeInBytes - 1

byteRangeToTaskRange :: FilePath -> Integer -> Integer -> Range -> TaskRange
byteRangeToTaskRange basePath 1 rangeIdx range = TaskRange basePath range rangeIdx
byteRangeToTaskRange basePath totalCnt rangeIdx range | totalCnt > 1 = TaskRange (basePath ++ ".part" ++ show rangeIdx) range rangeIdx
byteRangeToTaskRange _ _ _ _ = error "illegal count"

createTask :: Text -> FilePath -> Integer -> IO Task
createTask url path threadCnt = do
  manager <- HC.newManager HC.tlsManagerSettings
  request <- HC.parseRequest . T.unpack $ url
  taskInfo <- getTaskInfo url request manager
  let size = taskInfo ^. totalInBytes
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
  flip ST.evalStateT initialState $ do
    chan <- liftIO STM.newTChanIO

    void $
      liftIO $
        forM_
          (zip [0 ..] ranges)
          ( \(workerId, TaskRange tempPath range _) ->
              forkIO $ do
                c' <- STM.atomically $ do
                  c' <- STM.dupTChan chan
                  STM.writeTChan c' $ WorkerStartRunningEvent workerId
                  return c'
                downloadRangeRetry workerId request range tempPath manager c' R.retryPolicyDefault
          )

    whileM_
      ( do
          s <- ST.get
          return $ any (\x -> let st = x ^. status in st == Ready || st == Running) (s ^. workerStates)
      )
      ( do
          e <-
            liftIO $
              STM.atomically $
                STM.readTChan chan

          s <- ST.get

          ST.put
            ( case e of
                WorkerStartRunningEvent i -> s & (workerStates . ix i . status .~ Running)
                WorkerProgressUpdateEvent i _ n ->
                  let s' = s & (workerStates . ix i . now .~ n)
                      n' = sumOf (workerStates . each . now) s' :: Integer
                   in now .~ n' $ s'
                WorkerDoneEvent i r -> s & (workerStates . ix i . status .~ Done)
                WorkerFailedEvent i ex -> s & (workerStates . ix i . status .~ Failed)
            )

          s' <- ST.get

          liftIO $ print $ F.sformat (F.bytes (F.fixed 2)) (s' ^. now)
      )

    void $
      liftIO $ do
        let files = ranges ^.. each . savePath
        case length files of
          0 -> error "illegal temp files count"
          1 -> return ()
          _ -> do
            mergeFiles path files
            forM_ files removeFile
  where
    initialState :: TaskRunningState
    initialState =
      TaskRunningState
        { _trsNow = 0,
          _trsTotal = size,
          _trsWorkers = length ranges,
          _trsWorkerStates =
            zipWith (curry (\(workerId, TaskRange _ (Range a b) _) -> TaskRunningWorkerState workerId 0 (b - a) Ready)) [0 ..] ranges
        }

download :: Text -> FilePath -> Integer -> IO ()
download url path threadCount = do
  task <- createTask url path threadCount
  void $ doTask task
