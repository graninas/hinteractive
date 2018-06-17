module ZorkLike.Scenarios where

import           Control.Monad             (unless, void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.Trans.State (evalStateT)
import qualified Data.ByteString.Char8     as BS
import qualified Data.Map                  as Map

import           Hinteractive
import           ZorkLike.Objects

-- A Zork-like game demo.
-- This module contains the graph of locations.
westOfHouse' :: (Bool, Bool) -> AdventureL ()
westOfHouse' (showDescr, showMailbox) = do
  mailbox :: Mailbox <- getObject "mailbox"
  when showDescr   $ printMessage "West of House"
  when showDescr   $ printMessage "This is an open field west of a white house, with a boarded front door."
  when showMailbox $ printMessage $ describeObject mailbox
  when showDescr   $ printMessage "A rubber mat saying 'Welcome to Zork!' lies by the door."
