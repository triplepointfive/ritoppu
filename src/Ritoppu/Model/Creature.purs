module Ritoppu.Model.Creature
  ( Creature
  , creatureName
  ) where

import Prelude

import Ritoppu.Model.Stats (Stats)
import Ritoppu.Model.CreatureType (CreatureType)

type Creature = { type :: CreatureType, stats :: Stats }

creatureName :: Creature -> String
creatureName creature = show creature.type
