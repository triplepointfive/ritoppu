module Ritoppu.Action.Move
  ( move
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Ritoppu.Action (Action(..), ActionResult, addAction, withAction)
import Ritoppu.Action.CreatureAct (creatureAct)
import Ritoppu.Model (Creature, Direction, Game, Point, availableToMoveTo, creatureAt, creatureName, damageTo)
import Ritoppu.Mutation (moveTo, removeCreature, takeDamage, updateCreature, updateFov)

move :: Direction -> Game -> ActionResult Game
move dir game@{ stage } = case creatureAt stage dest of
  Just creature -> attack dest creature game

  -- FIX: move turn to function
  _ | availableToMoveTo stage dest
      -> creatureAct $ game { stage = updateFov stage { player { pos = dest, turn = stage.player.turn + 5 } } }
  _ -> withAction game (LogMessage "Hit a wall")

  where

  dest = moveTo dir stage.player.pos

-- TODO: Move creatureAct out of here
attack :: Point -> Creature -> Game -> ActionResult Game
attack pos creature game = case damage of
  0 ->
    addAction (LogMessage (
      "You attacked " <> creatureName creature <> " but did no damage"))
      $ creatureAct game
  _ | damage >= creature.stats.hp ->
    -- TODO: Leave corpse
    addAction (LogMessage (
      "You smashed " <> creatureName creature))
      $ creatureAct game { stage = removeCreature pos game.stage }
  _ ->
    addAction (LogMessage (
      "You attacked " <> creatureName creature <> " and did " <> show damage <> " damage"))
      $ creatureAct game
        { stage = updateCreature pos (\c -> c { stats = takeDamage damage c.stats }) game.stage
        }

  where

  damage = damageTo game.stage.player.stats creature.stats
