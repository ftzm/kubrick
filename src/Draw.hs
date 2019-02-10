module Draw
  ( drawUI
  ) where

import Brick hiding (raw)
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Graphics.Vty
import Data.Monoid
import Data.Maybe
import qualified Data.Vector as V
import Control.Lens

import AppState
import AppView
import Objects.Pod
import Objects.PodList
import Objects.ObjectMeta
import History (now, lookForward)

listDrawElement ::  Bool -> Pod -> Widget Name
listDrawElement sel a =
    let selStr s = if sel
                   then (str $ "-> " <> s)
                   else (str $ "   " <> s)
    in selStr $ show (a ^. metadata . name)

drawPanes :: Widget Name -> Maybe (Widget Name) -> [Widget Name]
drawPanes w1 Nothing = [C.vCenter $ C.hCenter w1]
drawPanes w1 (Just w2) = [C.vCenter $ C.hCenter w1 <+> C.hCenter w2]

drawPodList :: Int -> V.Vector Pod -> Widget Name
drawPodList i ps = box
  where
    l = L.listMoveTo i $ L.list PodListViewW ps 1
    box = B.borderWithLabel (str "PodList") $
          L.renderList listDrawElement True l

drawPod :: String -> V.Vector Pod -> Widget Name
drawPod n ps = str
             $ maybe "No such pod."
             (view raw) (V.find ((n ==) . view (metadata . name)) ps)

drawUI :: AppState -> [Widget Name]
drawUI s = drawPanes w1 w2
  where
    view = now $ s ^. history
    w1
      | PodListView i <- view = drawPodList i (s ^. apiObjects . podList)
      | PodView n <- view = drawPod n (s ^. apiObjects . podList)
    w2 = Just $ str "other pane"

nextView :: AppState -> Maybe AppView
nextView s
  | PodListView i <- nowView = do
      let n = fromMaybe "" $ s ^? apiObjects . podList . ix i . metadata . name
      return $ lookForward (PodView n) (s ^. history)
  where nowView = now $ s ^. history
