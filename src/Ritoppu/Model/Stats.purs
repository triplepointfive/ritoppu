module Ritoppu.Model.Stats
  ( Stats
  , damageTo
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
