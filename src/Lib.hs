module Lib where
import Control.Monad.Trans.State (StateT)
import Conduit (MonadIO(liftIO))
import Control.Concurrent.STM (newTChanIO)

lib :: StateT Int IO ()
lib = do
    chan <- liftIO $ newTChanIO 
    return ()
