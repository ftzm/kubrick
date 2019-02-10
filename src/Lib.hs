{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib
    ( runKubrick
    ) where


import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Brick
import Graphics.Vty

import AppView
import AppState
import Http
import Events
import Draw
import History

app :: App AppState () Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const (attrMap defAttr [])
  }

initialApiObjects = do
  _podList <- getPods
  return ApiObjects{..}

initialState :: IO AppState
initialState = do
  let _splitPane = False
  _apiObjects <- initialApiObjects
  let _history = initialHistory
  let _currentView = PodListView 0
  return AppState{..}

refreshPods :: AppState -> EventM Name (Next AppState)
refreshPods s = do
    pl <- liftIO getPods
    continue $ s & apiObjects . podList .~ pl

runKubrick :: IO ()
runKubrick = void $ defaultMain app =<< initialState
