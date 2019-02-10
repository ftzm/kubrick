module Events
  ( handleEvent
  ) where

import Control.Monad.IO.Class
import Control.Lens
import Data.Maybe
import Data.Tree.Zipper
import Brick
import Graphics.Vty
import qualified Data.Vector as V

import AppState
import AppView
import History
import Objects.Pod
import Objects.ObjectMeta
import Http

------------------------------------------------------------
-- Top-level event router

handleEvent :: AppState -> BrickEvent Name () -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvResize x y))
  | x < 40 = halt s
  | otherwise = continue s
handleEvent s (VtyEvent (EvKey (KChar c) []))
  | 'q' <- c = halt s
  | 'h' <- c = continue $ s & history %~ historyBack
  | c == 'j' = continue $ s & history %~ modifyLabel (listMove s succ)
  | c == 'k' = continue $ s & history %~ modifyLabel (listMove s pred)
handleEvent s b
  | PodListView i <- view = podListEvent i s b
  | PodView {} <- view = podListEvent 1 s b
  where view = now $ s ^. history

------------------------------------------------------------
-- PodList Events

podListEvent :: Int -> AppState -> BrickEvent Name () -> EventM Name (Next AppState)
--podListEvent s (VtyEvent (EvKey (KChar 'j') [])) =
podListEvent i s (VtyEvent (EvKey (KChar c) []))
--  | c == 'j' = continue $ s & (appViews . podListView) . _Just %~ L.listMoveDown
  | c == 'r' = refreshPods s
  | c == 'l' = continue $ s & history %~ historyForward (PodView $ fromMaybe "" n)
  where n = s ^? apiObjects . podList . ix i . metadata . name

refreshPods :: AppState -> EventM Name (Next AppState)
refreshPods s = do
    pl <- liftIO getPods
    continue $ s & apiObjects . podList .~ pl

listMove :: AppState -> (Int -> Int) -> AppView -> AppView
listMove s f v
  | PodListView i <- v = PodListView $ adjust i
  where
    obj = case v of
      PodListView {} -> s ^. apiObjects . podList
      PodView {} -> s ^. apiObjects . podList
    adjust i = min (if f i >= 0 then f i else 0) (V.length obj - 1)

------------------------------------------------------------
-- Pod Events
