{-# LANGUAGE ScopedTypeVariables #-}

module ZorkLike.Locations where

import           Control.Monad             (unless, void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.Trans.State (evalStateT)
import qualified Data.ByteString.Char8     as BS
import qualified Data.Map                  as Map

import           Hinteractive
import           ZorkLike.Init             (inititalState)
import           ZorkLike.Objects
import           ZorkLike.Scenarios

-- | Game locations graph.
type AGGraph a = Graph AdventureL a

-- | Transient location that does nothing exept
-- evaluates the opening action on the mailbox.
-- NOTE: It's inconvenient to specify actions as transient locations.
-- TODO: investigate a better approach.
openMailbox :: (Bool, Bool) -> AGGraph ()
openMailbox houseView = graph $
  with (evalAction MailboxType "open" "mailbox" >> inputOnly houseView)
    -/> westOfHouse

-- | West of House location.
westOfHouse :: AGGraph (Bool, Bool)
westOfHouse = graph $
  with1 (\x -> westOfHouse' x >> getInput)
    ~> on "open mailbox" (openMailbox (False, False))
    /> leaf nop
