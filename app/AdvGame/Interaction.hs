{-# LANGUAGE MultiParamTypeClasses #-}

module AdvGame.Interaction where

import           Control.Lens      ((.~), (^.))
import           Control.Lens.TH   (makeClassy, makeFieldsNoPrefix, makeLenses)
import           Data.Aeson        (FromJSON, ToJSON)
import           Data.Either       (Either)
import qualified Data.Map          as Map
import           Data.Maybe        (Maybe (..))
import           GHC.Generics      (Generic)

import           Lib               (Event)

import           AdvGame.Container
import           AdvGame.Lang

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
