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

type ObjectName = String

-- | Generalized ADT for game mechanics language.
data AdventureLF next where
  -- | General game mechanics actions.
  GetUserInput :: (String -> next) -> AdventureLF next
  PrintMessage :: String  -> next  -> AdventureLF next

  -- | Inventory actions (draft).
  PutItem   :: Item -> next -> AdventureLF next
  DropItem  :: Item -> next -> AdventureLF next
  ListItems ::         next -> AdventureLF next

  -- | Game object state actions.
  GetObjSt :: FromJSON a => ObjectName -> (a -> next) -> AdventureLF next
  PutObjSt :: ToJSON a   => ObjectName -> a -> next -> AdventureLF next

-- | Functor for AdventureLF.
instance Functor AdventureLF where
  fmap f (GetUserInput   nextF)     = GetUserInput   (f . nextF)
  fmap f (PrintMessage s next)      = PrintMessage s (f next)

  fmap f (PutItem      s next)      = PutItem      s (f next)
  fmap f (DropItem     s next)      = DropItem     s (f next)
  fmap f (ListItems      next)      = ListItems      (f next)

  fmap f (GetObjSt name    nextF)   = GetObjSt name       (f . nextF)
  fmap f (PutObjSt name objSt next) = PutObjSt name objSt (f next)

-- | Free monad for game mechanics language.
type AdventureL = Free AdventureLF

-- General game mechanics.

-- | Requests a string from the console.
getUserInput :: AdventureL String
getUserInput = liftF $ GetUserInput id

-- | Prints a message to the console.
printMessage :: String -> AdventureL ()
printMessage message = liftF $ PrintMessage message ()

-- Inventory

-- | Puts item into the inventory.
putItem :: String -> AdventureL ()
putItem s = liftF $ PutItem s ()

-- | Drops item.
dropItem :: String -> AdventureL ()
dropItem s = liftF $ DropItem s ()

-- | List items in the inventory.
listItems :: AdventureL ()
listItems = liftF $ ListItems ()
