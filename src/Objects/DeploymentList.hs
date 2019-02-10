{-# LANGUAGE TemplateHaskell #-}

module Objects.DeploymentList where

import qualified Data.Text as T
import Control.Lens
import Data.Aeson
import Data.Aeson.TH

data DeploymentList = DeploymentList
  { _kind :: T.Text
  }
deriveJSON defaultOptions {fieldLabelModifier = drop 1 } ''DeploymentList
makeLenses ''DeploymentList
