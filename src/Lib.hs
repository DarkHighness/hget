module Lib where
import Control.Monad.Trans.State (StateT, get)
import Conduit (MonadIO(liftIO), MonadTrans (lift))
import Control.Concurrent.STM (newTChan, newTChanIO)

lib :: StateT Int IO ()
lib = do
    chan <- liftIO $ newTChanIO 
    return ()
