module Ritoppu.Mutation.Player
  ( gainXp
  ) where

import Prelude

import Ritoppu.Model (Player)

gainXp :: Int -> Player -> Player
gainXp xp player = player { level { currentXp = player.level.currentXp + xp } }
