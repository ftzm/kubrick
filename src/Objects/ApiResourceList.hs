{-# LANGUAGE TemplateHaskell #-}

module Objects.ApiResourceList where

import qualified Data.Text as T
import Control.Lens
import Data.Aeson
import Data.Aeson.TH

import Objects.Resource

data ApiResourceList = ApiResourceList
  { _kind :: T.Text
  , _resources :: [Resource]
  }
deriveJSON defaultOptions {fieldLabelModifier = drop 1 } ''ApiResourceList
makeLenses ''ApiResourceList
