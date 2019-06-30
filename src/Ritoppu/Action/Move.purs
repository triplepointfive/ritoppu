module Ritoppu.Action.Move
  ( move
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Ritoppu.Action (Action(..), ActionResult, inactive, withAction)
import Ritoppu.Model (Direction, Game, availableToMoveTo, creatureName, creatureAt)
import Ritoppu.Mutation (moveTo, updateFov)

move :: Direction -> Game -> ActionResult Game
move dir game@{ stage } = case creatureAt stage dest of
  Just creature ->
    withAction game (LogMessage (
      "You kick the " <> creatureName creature <> " in the shins, much to its annoyance!"))

  _ | availableToMoveTo stage dest
      -> inactive $ game { stage = updateFov stage { player { pos = dest } } }
  _ -> withAction game (LogMessage "Hit a wall")

  where

  dest = moveTo dir stage.player.pos
