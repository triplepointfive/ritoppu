module Ritoppu.Action.Move
  ( move
  ) where

import Prelude

import Ritoppu.Action (ActionResult, inactive)
import Ritoppu.Model (Direction, Game)
import Ritoppu.Mutation (moveTo)

move :: Direction -> Game -> ActionResult Game
move dir game = inactive $ game { stage { player { pos = dest } } }

  where

  dest = moveTo dir game.stage.player.pos
