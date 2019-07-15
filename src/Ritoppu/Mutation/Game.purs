module Ritoppu.Mutation.Game
  ( onStage
  ) where

import Ritoppu.Model (Stage, Game)

onStage :: (Stage -> Stage) -> Game -> Game
onStage f game = game { stage = f game.stage }
