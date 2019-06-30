module Ritoppu.Model.CreatureType
  ( CreatureType(..)
  ) where

import Prelude

data CreatureType
  = Orc

instance showCreatureType :: Show CreatureType where
  show = case _ of
    Orc -> "Orc"
