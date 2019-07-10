module Ritoppu.Mutation.Stage
  ( setTile
  , updateFov
  , addCreature
  , removeCreature
  , moveCreature
  , updateCreature
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Ritoppu.Model (Point, Stage, Tile, Creature, availableToMoveTo)
import Ritoppu.Mutation.FovMask (rebuildFov)

setTile :: Tile -> Point -> Stage  -> Stage
setTile tile pos stage = stage { tiles = Map.insert pos tile stage.tiles }

updateFov :: Stage -> Stage
updateFov stage = stage
  { fovMask = rebuildFov 10 stage.player.pos (availableToMoveTo stage) stage.fovMask
  }

addCreature :: Point -> Creature -> Stage -> Stage
addCreature pos creature stage =
  stage { creatures = Map.insert pos creature stage.creatures }

removeCreature :: Point -> Stage -> Stage
removeCreature pos stage =
  stage { creatures = Map.delete pos stage.creatures }

moveCreature :: Point -> Point -> Creature -> Stage -> Stage
moveCreature from dest creature = addCreature dest creature <<< removeCreature from

updateCreature :: Point -> (Creature -> Creature) -> Stage -> Stage
updateCreature pos f stage
  = stage { creatures = Map.update (Just <<< f) pos stage.creatures }
