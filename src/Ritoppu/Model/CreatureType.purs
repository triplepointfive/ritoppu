module Ritoppu.Model.CreatureType
  ( CreatureType(..)
  ) where

import Prelude

data CreatureType
  = RedNagaHatchling
  | RedNaga

instance showCreatureType :: Show CreatureType where
  show = case _ of
    RedNagaHatchling -> "Red naga hatchling"
    RedNaga -> "Red naga"

derive instance eqCreatureType :: Eq CreatureType
derive instance ordCreatureType :: Ord CreatureType
