module Ritoppu.Action.Move
  ( move
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Ritoppu.Action (Action(..), ActionResult, addAction, withAction)
import Ritoppu.Action.CreatureAct (creatureAct)
import Ritoppu.Model (Creature, Direction, Game, Point, availableToMoveTo, creatureAt, creatureName, damageTo, newCorpse)
import Ritoppu.Mutation (moveTo, removeCreature, takeDamage, updateCreature, updateFov, addItem)

move :: Direction -> Game -> ActionResult Game
move dir game@{ stage } = case creatureAt stage dest of
  Just creature -> attack dest creature game

  -- FIX: move turn to function
  -- TODO: Add you see log message
  _ | availableToMoveTo stage dest
      -> creatureAct $ playerTurn $ game { stage = updateFov stage { player { pos = dest } } }
  _ -> withAction game (LogMessage "Hit a wall")

  where

  dest = moveTo dir stage.player.pos

-- TODO: Move creatureAct out of here
attack :: Point -> Creature -> Game -> ActionResult Game
attack pos creature game = case damage of
  0 ->
    addAction (LogMessage (
      "You attacked " <> creatureName creature <> " but did no damage"))
      $ creatureAct $ playerTurn game
  _ | damage >= creature.stats.hp ->
    -- TODO: Leave corpse
    addAction (LogMessage (
      "You smashed " <> creatureName creature))
      $ creatureAct $ playerTurn game { stage
          = addItem pos (newCorpse creature)
          $ removeCreature pos game.stage }
  _ ->
    addAction (LogMessage (
      "You attacked " <> creatureName creature <> " and did " <> show damage <> " damage"))
      $ creatureAct $ playerTurn game
        { stage = updateCreature pos (\c -> c { stats = takeDamage damage c.stats }) game.stage
        }

  where

  damage = damageTo game.stage.player.stats creature.stats

playerTurn :: Game -> Game
playerTurn game = game { stage { player { turn = game.stage.player.turn + 5 } } }
