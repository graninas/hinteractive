{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module AdvGame.Lang where

import           Control.Lens.TH           (makeFieldsNoPrefix, makeLenses)
import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Exists
import qualified Data.Map                  as Map
import           Data.Maybe                (Maybe (..))

type Item = String

type ObjectModifier a = a -> Maybe a

type OnSuccessAction a = a -> AdventureL ()
type OnFailAction    a = a -> AdventureL ()

data Action a  = Action (ObjectModifier a) (OnSuccessAction a) (OnFailAction a)
type Actions a = Map.Map String (Action a)

data Object objType objSt = Object
  { _name    :: String
  , _state   :: objSt
  , _actions :: Actions objSt
  }

class FromJSON objSt => ToObject objType objSt | objType -> objSt, objSt -> objType where
  object :: objSt -> Object objType objSt

data AdventureLF next where
  GetUserInput :: (String -> next) -> AdventureLF next
  PrintMessage :: String  -> next  -> AdventureLF next
  PutItem      :: Item    -> next  -> AdventureLF next
  DropItem     :: Item    -> next  -> AdventureLF next
  ListItems    ::            next  -> AdventureLF next
  GetObj       :: FromJSON a => String -> (a -> next) -> AdventureLF next
  PutObj       :: ToJSON a   => String -> a -> next -> AdventureLF next

type AdventureL = Free AdventureLF

instance Functor AdventureLF where
  fmap f (GetUserInput   nextF)   = GetUserInput   (f . nextF)
  fmap f (PrintMessage s next)    = PrintMessage s (f next)
  fmap f (PutItem      s next)    = PutItem      s (f next)
  fmap f (DropItem     s next)    = DropItem     s (f next)
  fmap f (ListItems      next)    = ListItems      (f next)
  fmap f (GetObj name    nextF)   = GetObj name    (f . nextF)
  fmap f (PutObj name objSt next) = PutObj name objSt (f next)

getUserInput :: AdventureL String
getUserInput = liftF $ GetUserInput id

printMessage :: String -> AdventureL ()
printMessage message = liftF $ PrintMessage message ()

putItem :: String -> AdventureL ()
putItem s = liftF $ PutItem s ()

dropItem :: String -> AdventureL ()
dropItem s = liftF $ DropItem s ()

listItems :: AdventureL ()
listItems = liftF $ ListItems ()

-- TODO: move to type literals

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

makeFieldsNoPrefix ''Object
