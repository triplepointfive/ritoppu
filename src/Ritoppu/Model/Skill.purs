module Ritoppu.Model.Skill
  ( Skill
  , skillValue
  ) where

import Data.Int (toNumber)

import Ritoppu.Model.Attribute (AttributeType)

type Skill =
  { own :: Int
  , progress :: Number
  , progressSpeed :: Number
  , governedBy :: AttributeType
  }

skillValue :: Skill -> Number
skillValue skill = toNumber skill.own
