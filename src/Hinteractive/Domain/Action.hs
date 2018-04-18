module Hinteractive.Domain.Action where

import qualified Data.Map                       as Map
import           Data.Maybe                     (Maybe (..))

import           Hinteractive.Language.Language (AdventureL)

type ObjectModifier a = a -> Maybe a

type OnSuccessAction a = a -> AdventureL ()
type OnFailAction    a = a -> AdventureL ()

data Action a  = Action (ObjectModifier a) (OnSuccessAction a) (OnFailAction a)
type Actions a = Map.Map String (Action a)
