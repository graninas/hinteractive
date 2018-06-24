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
-- evaluating the opening action on the mailbox.
-- NOTE: It's inconvenient to specify actions as transient locations.
-- TODO: investigate a better approach.
openMailbox :: AGGraph ()
openMailbox = graph $
  with (evalAction MailboxType "open" "mailbox")
    -/> westOfHouse

-- | West of House location.
westOfHouse :: AGGraph ()
westOfHouse = graph $
  with (westOfHouse' >> getInput)
    ~> on "open mailbox" openMailbox
    /> leaf nop
