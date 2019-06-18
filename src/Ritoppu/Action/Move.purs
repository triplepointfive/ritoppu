module Ritoppu.Action.Move
  ( move
  ) where

import Prelude

import Ritoppu.Action (ActionResult, inactive)
import Ritoppu.Model (Direction, Game, availableToMoveTo)
import Ritoppu.Mutation (moveTo)

move :: Direction -> Game -> ActionResult Game
move dir game = case unit of
  _ | availableToMoveTo game.stage dest
      -> inactive $ game { stage { player { pos = dest } } }
  _ -> inactive game

  where

  dest = moveTo dir game.stage.player.pos
