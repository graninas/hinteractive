{-# LANGUAGE ScopedTypeVariables #-}

module ZorkLike.Game where

import           Control.Monad             (unless, void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.Trans.State (evalStateT)
import qualified Data.ByteString.Char8     as BS
import qualified Data.Map                  as Map

import           Hinteractive
import           ZorkLike.Init             (inititalState)
import           ZorkLike.Objects

type AGGraph a b = Graph AdventureL a b

openMailbox :: (Bool, Bool) -> AGGraph () ()
openMailbox houseView = graph $
  with (evalAction MailboxType "open" "mailbox" >> inputOnly houseView)
    -/> westOfHouse

westOfHouse :: AGGraph (Bool, Bool) ()
westOfHouse = graph $
  with1 (\x -> westOfHouse' x >> getInput)
    ~> on "open mailbox" (openMailbox (False, False))
    /> leaf nop

westOfHouse' :: (Bool, Bool) -> AdventureL ()
westOfHouse' (showDescr, showMailbox) = do
  mailbox :: Mailbox <- getObject "mailbox"
  when showDescr   $ printMessage "West of House"
  when showDescr   $ printMessage "This is an open field west of a white house, with a boarded front door."
  when showMailbox $ printMessage $ describeObject mailbox
  when showDescr   $ printMessage "A rubber mat saying 'Welcome to Zork!' lies by the door."

game :: AGGraph () ()
game = graph $
  with (inputOnly (True, True))
    -/> westOfHouse

runGame :: IO ()
runGame = evalStateT (runGraph' run (== "back") game) inititalState
