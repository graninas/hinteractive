{-# LANGUAGE ScopedTypeVariables #-}

module ZorkLike.Game where

import           Control.Monad             (unless, void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.Trans.State (evalStateT)
import qualified Data.ByteString.Char8     as BS
import qualified Data.Map                  as Map

import           Hinteractive
import           ZorkLike.Init             (inititalState)
import           ZorkLike.Locations
import           ZorkLike.Objects
import           ZorkLike.Scenarios

initialize :: AdventureL ()
initialize = createVariable "WestOfHouse" (WestOfHouseView True True)


-- | Initial location of the game.
-- Does nothing except passing an initial state into the next location (`westOfHouse`).
game :: AGGraph ()
game = graph $
  with initialize
    -/> westOfHouse

-- | Start a game with the initial state and default "back" event.
runGame :: IO ()
runGame = evalStateT (runGraph' run (== "back") game) inititalState
