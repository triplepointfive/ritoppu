module Ritoppu.Mutation.Stats
  ( takeDamage
  , heal
  ) where

import Prelude

import Ritoppu.Model (Stats)

takeDamage :: Int -> Stats -> Stats
takeDamage damage stats@{ hp } = stats { hp = hp - damage }

heal :: Int -> Stats -> Stats
heal amount stats@{ hp, maxHp } = stats { hp = min maxHp (hp + amount) }
