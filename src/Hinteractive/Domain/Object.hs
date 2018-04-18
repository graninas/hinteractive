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

data Object objType objSt = Object
  { _name    :: String
  , _state   :: objSt
  , _actions :: Actions objSt
  }

class FromJSON objSt => ToObject objType objSt | objType -> objSt, objSt -> objType where
  object :: objSt -> Object objType objSt


-- TODO: move to type literals
-- TODO: remove dependency from Language by introducing a type class for interaction.
-- Move these things back to Language.

getObject
  :: (FromJSON objSt, ToObject objType objSt)
  => String
  -> AdventureL (Object objType objSt)
getObject name = do
  objSt <- liftF $ GetObj name id
  pure $ object objSt

getObject'
  :: (FromJSON objSt, ToObject objType objSt)
  => objType
  -> String
  -> AdventureL (Object objType objSt)
getObject' _ = getObject

putObject
  :: ToJSON objSt
  => String
  -> objSt
  -> AdventureL ()
putObject name objSt = liftF $ PutObj name objSt ()
