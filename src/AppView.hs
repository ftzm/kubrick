{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module AppView where

import Data.Data
import Data.Map
import Control.Lens

data AppView
  = PodListView
    Int -- ^ Index of current item
  | PodView
    String -- ^ Name of pod to look up
  deriving
    ( Show
    , Eq
    , Ord
    )
makeClassyPrisms ''AppView

-- history as Zipper on Trie?

-- the trie method has a problem, as it lacks a notion of order of insertion
-- (probably, it seems implemented based on Map). With lists, the newest item
-- is prepended and so this can be taken as the default position when
-- previewing and moving to existing futures. Some alternative will be
-- necessary with Trie.

-- does a tree have a notion of order of insertion? I think so actually, the
-- docs mention positions.

resumeable :: AppView -> AppView -> Bool
resumeable PodListView {} PodListView {} = True
resumeable (PodView name1) (PodView name2) = name1 == name2
resumeable _ _ = False

resumeable' = undefined

data ApiData
  = Obj String Attn (Map String ApiData)
  | Item String

-- Will this work without view labels? it's fine for viewing, but doesn't lend
-- itself to view-specific keys, for example. Since I will need to manually
-- format the data from the API, there will be an opportunity to conveniently
-- insert View labels into specific structures if that's where I'd like to do
-- it. If it's not going to be record types then I can use the actual names of
-- the entities as labels for what things are also.

data Entity
  = PodList
  | Pod
  | PodMeta
  | Container

data Attn
  = OK
  | Warn
  | Error

-- Things I need in view: path to view, potential filter, location name,
-- currently selected item
-- View looks like: PodMeta ["podlist", "pod-1234", "podMeta"] (Maybe "focused") (Filter
-- Expression)

data View = View
  { _entity   :: Entity
  , _keys   :: [String]
  , _focus  :: Maybe String
  , _filter :: String
  }

-- Maybe store as record? could make it easier to deal with changing what might
-- be stored here.


-- filter expressions will need the location name in order to understand how to
-- mutate the data to be displayed. writing queries from scratch may be
-- undesireable; I might prefer to present a number of options depending on the context.

-- I want to perform different options depending on the current view.

-- I want the ability to view secrets.


-- Where is view used?
-- history, eventhandler, viewhandler

-- atm, no way to associate a view with a single string entry. Maybe problem
-- for extra functionality on env entries/secrets, but then they usually have a
-- dict defining them so it might be ok.


--------------
-- fundamental design orientation:
-- write an api that will be a generic representation of json that can be
-- automagicaly created from aeson json. also have a slot which will be
-- reserved for an adt that will represent particular points in the api. that
-- way each point in the api will have an optional signature. If I then provide
-- at some point a function (ApiLocation -> Widget) I get custom views, and
-- ApiLocation -> Key -> SomeAction then I get actions based on location.

-- maybe a type like this:
data JSONView a = JSONView a Value
