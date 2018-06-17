{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Hinteractive.Domain.Object where

import           Control.Monad.Free             (Free (..), foldFree, liftF)
import           Data.Aeson                     (FromJSON, ToJSON)
import qualified Data.Map                       as Map
import           Data.Maybe                     (Maybe (..))

import           Hinteractive.Domain.Action
import           Hinteractive.Language.Language (AdventureL, AdventureLF (..))

-- This module describes a generic interactive object that can have a state.
-- It also provides some functions to work with game state.

-- | A generic object with state of a particular type.
data Object objType objSt = Object
  { _name    :: String          -- ^ Name of the object
  , _state   :: objSt           -- ^ State of the object
  , _actions :: Actions objSt   -- ^ Actions available with the object
  }

-- | Allows to tie an object type with an object state type.
-- This allows to have different types for objects in the game scenarios.
-- The objType type is usually a singleton ADT.
-- The objSt type is ususally an ADT that describes the object's state.
class FromJSON objSt => ToObject objType objSt | objType -> objSt, objSt -> objType where
  object :: objSt -> Object objType objSt

-- Game mechanics responsible for manipulation with game state and game objects.

-- | Requests the object from the game state by its name.
getObject
  :: (FromJSON objSt, ToObject objType objSt)
  => String
  -> AdventureL (Object objType objSt)
getObject name = do
  objSt <- liftF $ GetObj name id
  pure $ object objSt

-- | Requests the object from the game state by its type.
getObject'
  :: (FromJSON objSt, ToObject objType objSt)
  => objType
  -> String
  -> AdventureL (Object objType objSt)
getObject' _ = getObject

-- | Stores the object into the game state by its name.
putObject
  :: ToJSON objSt
  => String
  -> objSt
  -> AdventureL ()
putObject name objSt = liftF $ PutObj name objSt ()


-- Things TODO:
-- * objType can be a type-level string literal. This will be more convenient
-- than a singleton ADT.
-- * Remove dependency from Language by introducing a type class for interaction.
-- * Move AdventureL actions into the Language context.
