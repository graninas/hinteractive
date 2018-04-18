module ZorkLike.Init where

import           Data.Aeson       (encode)
import qualified Data.Map         as Map

import           Hinteractive
import           ZorkLike.Objects

inititalState :: Runtime
inititalState = mkRuntime Map.empty initialObjectStates

initialObjectStates :: ObjectStates
initialObjectStates = Map.fromList
  [ ("mailbox", encode mailboxObj)
  ]
