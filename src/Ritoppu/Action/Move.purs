module Ritoppu.Action.Move
  ( move
  ) where

import Prelude

import Ritoppu.Action (Action(..), ActionResult, inactive, withAction)
import Ritoppu.Model (Direction, Game, availableToMoveTo)
import Ritoppu.Mutation (moveTo, updateFov)

move :: Direction -> Game -> ActionResult Game
move dir game = case unit of
  _ | availableToMoveTo game.stage dest
      -> inactive $ game { stage = updateFov game.stage { player { pos = dest } } }
  _ -> withAction game (LogMessage "Hit a wall")

  where

  dest = moveTo dir game.stage.player.pos
