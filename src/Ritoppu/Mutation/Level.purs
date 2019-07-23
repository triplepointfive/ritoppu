module Ritoppu.Mutation.Level
  ( levelIncrease
  ) where

import Prelude

import Ritoppu.Model (Level, experienceToNextLevel)

-- EXTRA: Might level up multiple times
levelIncrease :: Int -> Level -> Level
levelIncrease xp level@{ currentLevel, currentXp } =
  { currentLevel: currentLevel + 1
  , currentXp: currentXp + xp - (experienceToNextLevel level)
  }
