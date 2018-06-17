module Hinteractive.Domain.Action where

import qualified Data.Map                       as Map
import           Data.Maybe                     (Maybe (..))

import           Hinteractive.Language.Language (AdventureL)

-- This module describes the part of engine
-- responsible for processing of player's commands.

-- This is a draft mechanics.

-- TODO: create a language model (for English).

-- | Type for a function that possibly modifiers an object state.
type ObjectModifier a = a -> Maybe a

-- | Handler that works with the object if the user action was successful.
type OnSuccessAction a = a -> AdventureL ()
-- | Handler that works with the object if the user action was failed.
type OnFailAction    a = a -> AdventureL ()

-- | Action that can be performed with an object.
data Action a  = Action (ObjectModifier a) (OnSuccessAction a) (OnFailAction a)

-- | Object actions
type Actions a = Map.Map String (Action a)
