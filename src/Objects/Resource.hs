{-# LANGUAGE TemplateHaskell #-}

module Objects.Resource where

import qualified Data.Text as T
import Control.Lens
import Data.Aeson
import Data.Aeson.TH

data Resource = Resource
  { _name :: T.Text
  } deriving Show
deriveJSON defaultOptions {fieldLabelModifier = drop 1 } ''Resource
makeLenses ''Resource
