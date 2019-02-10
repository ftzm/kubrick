{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Objects.PodStatus where

import qualified Data.Text as T
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.TH
import Data.Map

data PodStatus = PodStatus
  { _message :: Maybe String
  , _hostIP :: Maybe String
  , _podIP :: Maybe String
  , _phase :: Maybe String
  , _qosClass :: Maybe String
  , _reason :: Maybe String
  , _startTime :: Maybe String
  } deriving Show
makeLenses ''PodStatus

instance FromJSON PodStatus where
  parseJSON = withObject "pod" $ \o -> do
    _message <- o .:? "message"
    _hostIP <- o .:? "hostIP"
    _podIP <- o .:? "podIP"
    _phase <- o .:? "phase"
    _qosClass <- o .:? "qosClass"
    _reason <- o .:? "reason"
    _startTime <- o .:? "startTime"
    let _raw  = show o
    return PodStatus{..}

instance ToJSON PodStatus where
  toJSON PodStatus{..} = object
    [ "message" .= _message
    ]
