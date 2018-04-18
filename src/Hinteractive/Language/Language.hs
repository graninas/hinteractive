{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Hinteractive.Language.Language where

import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Maybe                (Maybe (..))

import           Hinteractive.Domain.Item

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
