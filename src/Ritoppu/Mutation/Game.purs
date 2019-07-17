module Ritoppu.Mutation.Game
  ( onStage
  , hitPlayer
  ) where

import Ritoppu.Model (Stage, Game)
import Ritoppu.Mutation.Stats (takeDamage)

onStage :: (Stage -> Stage) -> Game -> Game
onStage f game = game { stage = f game.stage }

hitPlayer :: Int -> Game -> Game
hitPlayer damage game = game { stage { player { stats = takeDamage damage game.stage.player.stats } } }
