{-# LANGUAGE MultiParamTypeClasses #-}

module Hinteractive.Language.Interaction where

import           Control.Lens                   ((.~), (^.))
import           Data.Aeson                     (FromJSON, ToJSON)
import qualified Data.Map                       as Map
import           Data.Maybe                     (Maybe (..))
import           GHC.Generics                   (Generic)

import           TransitionGraph                (Event)

import           Hinteractive.Domain
import           Hinteractive.Language.Language
import           Hinteractive.Lens

-- This module contains a game mechanics to work with user input
-- and game objects.

-- | Requests user input and makes it an event.
getInput :: AdventureL (Event, ())
getInput = do
  userInput <- getUserInput
  pure (userInput, ())

-- | Does nothing, returns empty event and empty data.
nop :: AdventureL (Event, ())
nop = pure ("", ())

-- TODO: a better naming.
-- | Does nothing, returns value as some data.
dataOnly :: a -> AdventureL (Event, a)
dataOnly a = pure ("", a)

-- Evaluates an action over the game object.
evalAction
  :: (FromJSON objSt, ToJSON objSt, ToObject objType objSt)
  => objType
  -> String
  -> String
  -> AdventureL (Event, ())
evalAction objType actName objName = do
  obj <- getObject' objType objName
  case Map.lookup actName (obj ^. actions) of
    Nothing -> printMessage $ "Can't do " ++ actName ++ " with " ++ objName
    Just (Action act onSuccess onFail) -> case act (obj ^. state) of
      Nothing -> onFail (obj ^. state)
      Just newObjSt -> do
        onSuccess newObjSt
        putObject objName newObjSt
  nop
