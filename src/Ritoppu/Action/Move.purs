module Ritoppu.Action.Move
  ( move
  , wait
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Ritoppu.Action (Action(..), ActionResult, Message(..), addAction, withAction)
import Ritoppu.Action.CreatureAct (creatureAct)
import Ritoppu.Model (Creature, Direction, Game, Point, Stage, availableToMoveTo, creatureAt, damageTo, newCorpse)
import Ritoppu.Mutation (addItem, gainXp, moveTo, removeCreature, takeDamage, updateCreature, updateFov)

move :: Direction -> Game -> ActionResult Game
move dir game@{ stage } = case creatureAt stage dest of
  Just creature -> attack dest creature game

  -- FIX: move turn to function
  -- TODO: Add you see log message
  _ | availableToMoveTo stage dest
      -> creatureAct $ playerTurn $ game { stage = updateFov stage { player { pos = dest } } }
  _ -> withAction game (LogMessage HitAWallM)

  where

  dest = moveTo dir stage.player.pos

-- TODO: Move creatureAct out of here
attack :: Point -> Creature -> Game -> ActionResult Game
attack pos creature game = case damage of
  0 ->
    addAction (LogMessage (AttackHarmlessM creature))
      $ creatureAct $ playerTurn game
  _ | damage >= creature.stats.hp ->
    addAction (LogMessage (AttackKillM creature))
      $ creatureAct $ playerTurn game { stage
          = addExperience creature.xp $ addItem pos (newCorpse creature)
          $ removeCreature pos game.stage }
  _ ->
    addAction (LogMessage (AttackM creature damage))
      $ creatureAct $ playerTurn game
        { stage = updateCreature pos (\c -> c { stats = takeDamage damage c.stats }) game.stage
        }

  where

  damage = damageTo game.stage.player.stats creature.stats

playerTurn :: Game -> Game
playerTurn game = game { stage { player { turn = game.stage.player.turn + 5 } } }

addExperience :: Int -> Stage -> Stage
addExperience xp stage = stage { player = gainXp xp stage.player }

wait :: Game -> ActionResult Game
wait = creatureAct <<< playerTurn
