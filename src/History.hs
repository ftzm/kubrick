module History
  ( History
  , initialHistory
  , historyBack
  , historyForward
  , lookForward
  , now
  ) where

import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.List (find)
import Data.Tree.Zipper

import AppView

type History = TreePos Full AppView
type History' = TreePos Full AppView'

-- | Continuously apply a function to a Maybe value while the value is a Just,
-- returning the results in a list.
iterateJust :: (a -> Maybe a) -> Maybe a -> [a]
iterateJust f = maybe [] (\x -> x : iterateJust f (f x))

initialHistory :: History
initialHistory = fromTree $ pure $ PodListView 0

childrenPositions :: History -> [History]
childrenPositions h = iterateJust next $ firstChild h

findFuture :: AppView -> History -> Maybe History
findFuture a = find (resumeable a . label) . childrenPositions

historyBack :: History -> History
historyBack h = fromMaybe h $ parent h

historyForward :: AppView -> History -> History
historyForward a h = fromMaybe (insert (pure a) $ children h) $ findFuture a h

lookForward :: AppView -> History -> AppView
lookForward a = maybe a label . findFuture a

now :: History -> AppView
now = label
