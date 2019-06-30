module Ritoppu.Model.CreatureRepository
  ( CreatureRepository
  ) where

import Ritoppu.Model.Creature (Creature)

type CreatureRepository = (Int -> Creature)
