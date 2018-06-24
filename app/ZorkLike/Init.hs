module ZorkLike.Init where

import           Data.Aeson       (encode)
import qualified Data.Map         as Map

import           Hinteractive
import           ZorkLike.Objects

-- | Initial state of the game. Game objects are clear, player's inventory is empty.
inititalState :: Runtime
inititalState = mkRuntime Map.empty initialObjectStates Map.empty

-- | Initial state of all game objects.
initialObjectStates :: ObjectStates
initialObjectStates = Map.fromList
  [ ("mailbox", encode mailboxSt)
  , ("leaflet", encode leafletSt)
  ]
