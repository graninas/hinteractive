
module Hinteractive.Domain.Item where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map   as Map
import           Data.Maybe (Maybe (..))

-- This module describes types and mechanics
-- for game objects that can be carried by a player (items).

-- TODO: create game mechanics for items (it's a dummy type currently).

-- | Interactive item that can be taken by a player.
type Item = String
