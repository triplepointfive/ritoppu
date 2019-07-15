module Ritoppu.Model.Item
  ( Item(..)
  , newCorpse
  , itemName
  ) where

import Prelude

import Ritoppu.Model.Creature (Creature)
import Ritoppu.Model.CreatureType (CreatureType)

data Item
  = Corpse CreatureType
  | HealingPotion

newCorpse :: Creature -> Item
newCorpse creature = Corpse creature.type

itemName :: Item -> String
itemName = case _ of
  Corpse creature -> "Corpse of " <> show creature -- EXTRA: Use creature name here
  HealingPotion -> "Healing potion"

derive instance eqItem :: Eq Item
derive instance ordItem :: Ord Item
