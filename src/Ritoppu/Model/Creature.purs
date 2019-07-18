module Ritoppu.Model.Creature
  ( AiStrategy(..)
  , Creature
  , creatureName
  ) where

import Prelude

import Ritoppu.Model.Stats (Stats)
import Ritoppu.Model.CreatureType (CreatureType)

data AiStrategy
  = BasicAI
  | ConfusedAI Int AiStrategy

type Creature =
  { type :: CreatureType
  , stats :: Stats
  , turn :: Int
  , aiStrategy :: AiStrategy
  }

creatureName :: Creature -> String
creatureName creature = show creature.type
