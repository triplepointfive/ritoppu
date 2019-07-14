module Ritoppu.Model.Item
  ( Item(..)
  , newCorpse
  ) where

import Ritoppu.Model.Creature (Creature)
import Ritoppu.Model.CreatureType (CreatureType)

data Item
  = Corpse CreatureType
  | HealingPotion

newCorpse :: Creature -> Item
newCorpse creature = Corpse creature.type
