module Ritoppu.Mutation.Stage
  ( setTile
  ) where

import Data.Map as Map

import Ritoppu.Model (Point, Stage, Tile)

setTile :: Tile -> Point -> Stage  -> Stage
setTile tile pos stage = stage { tiles = Map.insert pos tile stage.tiles }
