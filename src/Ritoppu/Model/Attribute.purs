module Ritoppu.Model.Attribute
  ( Attribute
  , AttributeType(..)
  , attributeValue
  ) where

import Prelude

import Data.Int (toNumber)

data AttributeType
  = Endurance
  | Strength
  | Agility
  | Speed
  | Personality
  | Intelligence
  | Willpower
  | Luck

instance show :: Show AttributeType where
  show = case _ of
    Endurance -> "Endurance"
    Strength -> "Strength"
    Agility -> "Agility"
    Speed -> "Speed"
    Personality -> "Personality"
    Intelligence -> "Intelligence"
    Willpower -> "Willpower"
    Luck -> "Luck"

derive instance eqAttributeType :: Eq AttributeType

type Attribute =
  { own :: Int
  }

attributeValue :: Attribute -> Number
attributeValue attribute = toNumber attribute.own
