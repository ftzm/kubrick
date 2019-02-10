{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Objects.ObjectMeta where

import qualified Data.Text as T
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.TH
import Data.Map

data ObjectMeta = ObjectMeta
  { _name :: String
  , _generateName :: Maybe String
  , _namespace :: Maybe String
  , _selfLink :: Maybe String
  , _uid :: Maybe String
  , _resourceVersion :: Maybe String
  , _creationTimestamp :: Maybe String
  , _labels :: Maybe (Map String String)
  } deriving Show
makeLenses ''ObjectMeta

instance FromJSON ObjectMeta where
  parseJSON = withObject "pod" $ \o -> do
    _name <- o .: "name"
    _generateName <- o .:? "generateName"
    _namespace <- o .:? "namespace"
    _selfLink <- o .:? "selfLink"
    _uid <- o .:? "uid"
    _resourceVersion <- o .:? "resourceVersion"
    _creationTimestamp <- o .:? "creationTimeStamp"
    _labels <- o .:? "labels"
    let _raw  = show o
    return ObjectMeta{..}

instance ToJSON ObjectMeta where
  toJSON ObjectMeta{..} = object
    [ "name" .= _name
    ]
