module Main where

import           ZorkLike.Game (runGame)

-- This is a Zork-like game to demonstrate the `hinteractive` engine possibilities.

main :: IO ()
main = runGame

-- Things TODO:
-- * Create a better structure for game assets (locations, scenarios, objects).
-- * Add save / load.
-- * Add more locations, scenarios, objects.
-- * Investigate a better approaches to create locations with scenarios
-- (it's a bit boilerplaty now.)
-- * Make it not exit when the action is invalid.
-- * Finish a demo game with several locations.
