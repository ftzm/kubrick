{-# LANGUAGE TemplateHaskell #-}

module Objects.PodList where

import qualified Data.Text as T
import Control.Lens
import Data.Aeson
import Data.Aeson.TH

import Objects.Pod

data PodList = PodList
  { _kind :: T.Text
  , _items :: [Pod]
  } deriving Show
deriveJSON defaultOptions {fieldLabelModifier = drop 1 } ''PodList
makeLenses ''PodList
