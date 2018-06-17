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
import           Hinteractive.Language.Language (AdventureL, AdventureLF (..),
                                                 ObjectName)

-- This module describes a generic interactive object that can have a state.
-- It also provides some functions to work with game state.

-- | A generic object with state of a particular type.
data Object objType objSt = Object
  { _name    :: ObjectName      -- ^ Name of the object
  , _state   :: objSt           -- ^ State of the object
  , _actions :: Actions objSt   -- ^ Actions available with the object
  }

-- | Allows to tie an object type with an object state type.
-- This simplifies working with object types.
-- Instead of specifying a type in a function signature,
-- you can pass a value of `objType`.
--
-- The objType type represents a class of objects having objSt state.
-- objType is usually a singleton ADT.
class FromJSON objSt => ToObject objType objSt | objType -> objSt, objSt -> objType where
  object :: objSt -> Object objType objSt

-- Game mechanics responsible for manipulation with game state and game objects.

-- | Requests the object from the game state by its name.
-- You need to specify objType and objSt.
getObject
  :: (FromJSON objSt, ToObject objType objSt)
  => ObjectName  -- ^ Object name as key
  -> AdventureL (Object objType objSt)
getObject name = do
  objSt <- liftF $ GetObjSt name id
  pure $ object objSt

-- | Requests the object from the game state by its type.
-- You need to pass objType. objSt will be deduced from the context.
getObject'
  :: (FromJSON objSt, ToObject objType objSt)
  => objType     -- ^ Object type to map to object state type
  -> ObjectName  -- ^ Object name as key
  -> AdventureL (Object objType objSt)
getObject' _ = getObject

-- | Stores the object into the game state by its name.
putObject
  :: ToJSON objSt
  => ObjectName   -- ^ Object name as key
  -> objSt        -- ^ Object state type
  -> AdventureL ()
putObject name objSt = liftF $ PutObjSt name objSt ()


-- Things TODO:
-- * objType can be a type-level string literal. This will be more convenient
-- than a singleton ADT.
-- * Remove dependency from Language by introducing a type class for interaction.
-- * Move AdventureL actions into the Language context.
