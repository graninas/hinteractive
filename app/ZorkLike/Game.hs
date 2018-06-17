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
import           ZorkLike.Scenarios
import           ZorkLike.Locations

-- | Initial location of the game.
-- Does nothing except passing an initial state into the next location (`westOfHouse`).
game :: AGGraph () ()
game = graph $
  with (inputOnly (True, True))
    -/> westOfHouse

-- | Start a game with the initial state.
runGame :: IO ()
runGame = evalStateT (runGraph' run (== "back") game) inititalState
