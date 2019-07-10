module Ritoppu.Mutation.Stats
  ( takeDamage
  ) where

import Prelude

import Ritoppu.Model (Stats)

takeDamage :: Int -> Stats -> Stats
takeDamage damage stats@{ hp } = stats { hp = hp - damage }
