{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Objects.Pod
  ( Pod
  , metadata
  , raw
  --, kind
  ) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T

import Objects.ObjectMeta
import Objects.PodStatus

data Pod = Pod
  { _metadata :: ObjectMeta
  , _status :: PodStatus
  --, _kind :: T.Text
  , _raw :: String
  } deriving Show
--deriveJSON defaultOptions {fieldLabelModifier = drop 1 } ''Pod
makeLenses ''Pod

instance FromJSON Pod where
  parseJSON = withObject "pod" $ \o -> do
    _metadata <- o .: "metadata"
    _status <- o .: "status"
   -- _kind <- o .: "kind"
    let _raw  = show o
    return Pod{..}

instance ToJSON Pod where
  toJSON Pod{..} = object
    [ "metadata" .= _metadata
    --, "kind" .= _kind
    ]
