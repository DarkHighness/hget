{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick (App (..), BrickEvent (..), EventM, Next, Widget)
import qualified Brick as B
import qualified Brick.AttrMap as B
import qualified Brick.BChan as BC
import qualified Brick.Main as B
import Control.Concurrent
import Control.Monad
import Graphics.Vty (Event (..), Key (..))
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as V

data HgetState = HgetState deriving (Show, Eq)

data HgetEvent = TickEvent

type ResourceName = String

hgetApp :: App HgetState HgetEvent ResourceName
hgetApp =
  App
    { appDraw = drawHgetUI,
      appChooseCursor = B.showFirstCursor,
      appHandleEvent = handleHgetEvent,
      appStartEvent = return,
      appAttrMap = const $ B.attrMap mempty []
    }

buildInitialState :: IO HgetState
buildInitialState = return HgetState

drawHgetUI :: HgetState -> [Widget ResourceName]
drawHgetUI st = [B.vBox [B.str "Hello"]]

handleHgetEvent :: HgetState -> BrickEvent ResourceName HgetEvent -> EventM ResourceName (Next HgetState)
handleHgetEvent s (AppEvent TickEvent) = B.continue s
handleHgetEvent s (VtyEvent (EvKey (KChar 'q') [])) = B.halt s
handleHgetEvent s _ = B.continue s

main :: IO ()
main = do
  chan <- BC.newBChan 10
  forkIO $
    forever $ do
      BC.writeBChan chan TickEvent
      threadDelay 100000
  initialState <- buildInitialState
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ B.customMain initialVty buildVty (return chan) hgetApp initialState
