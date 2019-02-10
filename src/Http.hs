module Http
  ( getPods
  ) where

import Network.Wreq
import qualified Data.Vector as V
import Data.Maybe
import Control.Lens
import Data.Aeson

import Objects.PodList
import Objects.Pod

getPodList :: IO PodList
getPodList = do
  r <- get "http://localhost:8001/api/v1/pods"
  let pl = fromJust $ decode $ r ^. responseBody :: PodList
  return pl

getPods :: IO (V.Vector Pod)
getPods = do
  pl <- getPodList
  let pods = V.fromList $ pl ^. items
  return pods
