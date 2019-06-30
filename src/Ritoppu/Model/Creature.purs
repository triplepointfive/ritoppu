module Ritoppu.Model.Creature
  ( Creature
  , creatureName
  ) where

import Prelude

import Ritoppu.Model.CreatureType (CreatureType)

type Creature = { type :: CreatureType }

creatureName :: Creature -> String
creatureName creature = show creature.type
