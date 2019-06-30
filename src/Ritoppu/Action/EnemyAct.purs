module Ritoppu.Action.EnemyAct
  ( creatureAct
  ) where

import Prelude

import Data.Foldable (foldr)
import Data.Int (round, toNumber)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Math (sqrt)
import Ritoppu.Action (Action(..), ActionResult, addAction, inactive, onResult)
import Ritoppu.Model (Game, Point, Stage, Creature, availableToMoveTo, creatureName)

creatureAct :: Game -> ActionResult Game
creatureAct game =
  foldr
      turn
      (inactive (game { stage { creatures = Map.empty :: Map.Map Point Creature } } ))
      ((Map.toUnfoldableUnordered game.stage.creatures) :: Array (Tuple Point Creature))

turn :: Tuple Point Creature -> ActionResult Game -> ActionResult Game
turn (Tuple pos creature) =
  onResult (\game -> game { stage = addCreature (moveForward pos game.stage) creature game.stage })

addCreature :: Point -> Creature -> Stage -> Stage
addCreature pos creature stage =
  stage { creatures = Map.insert pos creature stage.creatures }

moveForward :: Point -> Stage -> Point
moveForward origin stage@{ player: { pos } } = case unit of
  _ | availableToMoveTo stage dest && dest /= pos -> dest
  _ -> origin
  where

  dest = origin +
      { x: round (dx / distance)
      , y: round (dy / distance)
      }

  dx = toNumber (pos.x - origin.x)
  dy = toNumber (pos.y - origin.y)

  distance = sqrt (dx * dx + dy * dy)

