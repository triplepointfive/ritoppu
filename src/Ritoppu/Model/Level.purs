module Ritoppu.Model.Level
  ( Level
  , initLevel
  , isReadyToLevelUp
  , experienceToNextLevel
  ) where

import Prelude

type Level =
  { currentLevel :: Int
  , currentXp :: Int
  }

initLevel :: Level
initLevel = { currentLevel: 1, currentXp: 0 }

isReadyToLevelUp :: Int -> Level -> Boolean
isReadyToLevelUp xp level@{ currentXp } = xp + currentXp >= experienceToNextLevel level

experienceToNextLevel :: Level -> Int
experienceToNextLevel { currentLevel } = upBase + currentLevel * upFactor

  where

  upBase :: Int
  upBase = 200

  upFactor :: Int
  upFactor = 15
