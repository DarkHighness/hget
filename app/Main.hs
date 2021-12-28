{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick (App (..), BrickEvent (..), EventM, Next, Widget)
import qualified Brick as B
import qualified Brick.AttrMap as B
import qualified Brick.BChan as BC
import qualified Brick.Main as B
import qualified Brick.Widgets.Border as B
import Control.Concurrent
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Data.Fixed
import qualified Data.Text as T
import Data.Time (LocalTime (..), TimeOfDay (..), TimeZone, UTCTime)
import qualified Data.Time as TM
import qualified Data.Time.Clock.System as TM
import qualified Data.Time.Format.ISO8601 as T
import qualified Data.Time.LocalTime as TM
import Graphics.Vty (Event (..), Key (..))
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as V
import qualified Text.Printf as T

data HGetState = HGetState
  { _title :: String,
    _time :: TM.UTCTime,
    _zone :: TM.TimeZone
  }
  deriving (Show, Eq)

$(makeLenses ''HGetState)

data HGetEvent = TickEvent TM.UTCTime

type ResourceName = String

hgetApp :: App HGetState HGetEvent ResourceName
hgetApp =
  App
    { appDraw = drawHGetUI,
      appChooseCursor = B.showFirstCursor,
      appHandleEvent = handleHGetEvent,
      appStartEvent = return,
      appAttrMap = const $ B.attrMap mempty []
    }

buildInitialState :: IO HGetState
buildInitialState = do
  z <- TM.getCurrentTimeZone
  t <- TM.getCurrentTime
  return HGetState {_title = "HGet 0.0.0.1", _time = t, _zone = z}

formatTime :: TimeZone -> UTCTime -> String
formatTime z t = T.printf "%s %02d:%02d:%02d" (T.iso8601Show ld) todHour todMin (floor todSec :: Integer)
  where
    (LocalTime ld (TimeOfDay todHour todMin todSec)) = TM.utcToLocalTime z t

drawHGetUI :: HGetState -> [Widget ResourceName]
drawHGetUI st = [B.border $ B.padLeftRight 1 $ B.vBox [B.str $ st ^. title, B.str timeString]]
  where
    timeString = formatTime (st ^. zone) (st ^. time)

handleHGetEvent :: HGetState -> BrickEvent ResourceName HGetEvent -> EventM ResourceName (Next HGetState)
handleHGetEvent s (AppEvent (TickEvent t)) = B.continue $ time .~ t $ s
handleHGetEvent s (VtyEvent (EvKey (KChar 'q') [])) = B.halt s
handleHGetEvent s _ = B.continue s

main :: IO ()
main = do
  chan <- BC.newBChan 10
  forkIO $
    forever $ do
      t <- TM.getCurrentTime
      BC.writeBChan chan (TickEvent t)
      threadDelay 100000
  initialState <- buildInitialState
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ B.customMain initialVty buildVty (return chan) hgetApp initialState
