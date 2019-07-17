module Ritoppu.Model.Stats
  ( Stats
  , damageTo
  , isFullHealth
  ) where

import Prelude

type Stats =
  { maxHp :: Int
  , hp :: Int
  , defense :: Int
  , power :: Int
  }

damageTo :: Stats -> Stats -> Int
damageTo { power } { defense } = max 0 (power - defense)

isFullHealth :: Stats -> Boolean
isFullHealth { maxHp, hp } = hp >= maxHp
