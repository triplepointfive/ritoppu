module Ritoppu.Model.Creature
  ( Creature
  ) where

import Ritoppu.Model.CreatureType (CreatureType)

type Creature = { type :: CreatureType }
