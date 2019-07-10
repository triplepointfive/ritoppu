module Ritoppu.Action.Move
  ( move
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Ritoppu.Action (Action(..), ActionResult, addAction, withAction)
import Ritoppu.Action.CreatureAct (creatureAct)
import Ritoppu.Model (Direction, Game, availableToMoveTo, creatureName, creatureAt)
import Ritoppu.Mutation (moveTo, updateFov)

move :: Direction -> Game -> ActionResult Game
move dir game@{ stage } = case creatureAt stage dest of
  Just creature ->
    addAction (LogMessage (
      "You kick the " <> creatureName creature <> " in the shins, much to its annoyance!"))
      $ creatureAct game

  -- FIX: move turn to function
  _ | availableToMoveTo stage dest
      -> creatureAct $ game { stage = updateFov stage { player { pos = dest, turn = stage.player.turn + 5 } } }
  _ -> withAction game (LogMessage "Hit a wall")

  where

  dest = moveTo dir stage.player.pos
