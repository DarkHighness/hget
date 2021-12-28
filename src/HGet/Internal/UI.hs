{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HGet.Internal.UI where

import Brick (App (..), BrickEvent (..), EventM, Next, Widget)
import qualified Brick as B
import qualified Brick.AttrMap as B
import Brick.BChan (BChan)
import qualified Brick.BChan as BC
import qualified Brick.Main as B
import qualified Brick.Widgets.Border as B
import Control.Concurrent
import qualified Control.Concurrent as BC
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Data.Fixed
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (LocalTime (..), TimeOfDay (..), TimeZone, UTCTime)
import qualified Data.Time as TM
import qualified Data.Time.Clock.System as TM
import qualified Data.Time.Format.ISO8601 as TM
import qualified Data.Time.LocalTime as TM
import Formatting ((%))
import qualified Formatting as F
import Graphics.Vty (Event (..), Key (..))
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as V
import qualified HGet.Internal.CLI as CLI
import qualified Options.Applicative as CLI
import qualified Text.Printf as T

data HGetState = HGetState
  { _title :: Text,
    _time :: TM.UTCTime,
    _zone :: TM.TimeZone
  }
  deriving (Show, Eq)

$(makeLenses ''HGetState)

data HGetEvent = TickEvent TM.UTCTime

type ResourceName = Text

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

formatTime :: TimeZone -> UTCTime -> Text
formatTime z t = F.sformat (F.string % " " % F.int % ":" % F.int % ":" % F.int % "") (TM.iso8601Show ld) todHour todMin (floor todSec :: Int)
  where
    (LocalTime ld (TimeOfDay todHour todMin todSec)) = TM.utcToLocalTime z t

drawText :: Text -> Widget n
drawText = B.str . T.unpack

drawHGetUI :: HGetState -> [Widget ResourceName]
drawHGetUI st = [B.border $ B.padLeftRight 1 $ B.vBox [drawText $ st ^. title, drawText timeString]]
  where
    timeString = formatTime (st ^. zone) (st ^. time)

handleHGetEvent :: HGetState -> BrickEvent ResourceName HGetEvent -> EventM ResourceName (Next HGetState)
handleHGetEvent s (AppEvent (TickEvent t)) = B.continue $ time .~ t $ s
handleHGetEvent s (VtyEvent (EvKey (KChar 'q') [])) = B.halt s
handleHGetEvent s _ = B.continue s

buildTickChannel :: Int -> Int -> IO (BChan HGetEvent)
buildTickChannel buffer cycle = do
  chan <- BC.newBChan buffer
  forkIO $
    forever $ do
      t <- TM.getCurrentTime
      BC.writeBChan chan (TickEvent t)
      threadDelay cycle
  return chan

runUI :: IO HGetState
runUI = do
  config <- CLI.cliConfig
  chan <- buildTickChannel 10 100000
  initialState <- buildInitialState
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  B.customMain initialVty buildVty (return chan) hgetApp initialState