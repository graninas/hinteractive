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

getInput :: AdventureL (Event, ())
getInput = do
  userInput <- getUserInput
  pure (userInput, ())

nop :: AdventureL (Event, ())
nop = pure ("", ())

inputOnly :: a -> AdventureL (Event, a)
inputOnly a = pure ("", a)

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
