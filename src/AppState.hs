{-# LANGUAGE TemplateHaskell #-}

module AppState
  ( Name (..)
  , ApiObjects (..)
  , AppState (..)
  , splitPane
  , apiObjects
  , history
  , podList
  ) where

import qualified Data.Vector as V
import Control.Lens

import Objects.Pod
import History

data Name
  = PodListViewW
  | PodViewW
  deriving (Show, Eq, Ord)

data ApiObjects = ApiObjects
  { _podList :: V.Vector Pod
  }
makeLenses ''ApiObjects

data AppState = AppState
  { _splitPane :: Bool --rename to narrow or something
  , _apiObjects :: ApiObjects
  , _history :: History
  }
makeLenses ''AppState
