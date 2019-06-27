module Ritoppu.Mutation.Stage
  ( setTile
  , updateFov
  ) where

import Data.Map as Map
import Ritoppu.Model (Point, Stage, Tile, availableToMoveTo)
import Ritoppu.Mutation.FovMask (rebuildFov)

setTile :: Tile -> Point -> Stage  -> Stage
setTile tile pos stage = stage { tiles = Map.insert pos tile stage.tiles }

updateFov :: Stage -> Stage
updateFov stage = stage
    { fovMask = rebuildFov 10 stage.player.pos (availableToMoveTo stage) stage.fovMask
    }
