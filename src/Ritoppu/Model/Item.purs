module Ritoppu.Model.Item
  ( Item(..)
  , newCorpse
  , itemName
  ) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Either (Either(..))
import Ritoppu.Model.Creature (Creature)
import Ritoppu.Model.CreatureType (CreatureType)
import Ritoppu.Model.Equipment (MainHandItem, OffHandItem)

data Item
  = Corpse CreatureType
  | HealingPotion
  | LightningScroll
  | FireballScroll
  | ConfusionScroll
  | MainHandItem MainHandItem
  | OffHandItem OffHandItem

newCorpse :: Creature -> Item
newCorpse creature = Corpse creature.type

itemName :: Item -> String
itemName = case _ of
  Corpse creature -> "Corpse of " <> show creature -- EXTRA: Use creature name here
  HealingPotion -> "Healing potion"
  LightningScroll -> "Lightning Scroll"
  FireballScroll -> "Fireball Scroll"
  ConfusionScroll -> "Confusion Scroll"
  MainHandItem mainHandItem -> show mainHandItem
  OffHandItem offHandItem -> show offHandItem

derive instance eqItem :: Eq Item
derive instance ordItem :: Ord Item

instance decodeJsonItem :: DecodeJson Item where
  decodeJson json = do
    x <- decodeJson json
    item <- x .: "item"
    case item of
      "Corpse" -> do
        creatureType <- x .: "CreatureType"
        pure (Corpse creatureType)
      "MainHandItem" -> do
        mainHandItem <- x .: "MainHandItem"
        pure (MainHandItem mainHandItem)
      "OffHandItem" -> do
        offHandItem <- x .: "OffHandItem"
        pure (OffHandItem offHandItem)
      "HealingPotion" -> pure HealingPotion
      "LightningScroll" -> pure LightningScroll
      "FireballScroll" -> pure FireballScroll
      "ConfusionScroll" -> pure ConfusionScroll
      _ -> Left ("Unknown Item " <> item)

instance encodeJsonItem :: EncodeJson Item where
  encodeJson = case _ of
    Corpse creatureType ->
      "item" := "Corpse"
        ~> "CreatureType" := creatureType
        ~> jsonEmptyObject
    HealingPotion ->
      "item" := "HealingPotion"
        ~> jsonEmptyObject
    LightningScroll ->
      "item" := "LightningScroll"
        ~> jsonEmptyObject
    FireballScroll ->
      "item" := "FireballScroll"
        ~> jsonEmptyObject
    ConfusionScroll ->
      "item" := "ConfusionScroll"
        ~> jsonEmptyObject
    MainHandItem mainHandItem ->
      "item" := "MainHandItem"
        ~> "MainHandItem" := mainHandItem
        ~> jsonEmptyObject
    OffHandItem offHandItem ->
      "item" := "OffHandItem"
        ~> "OffHandItem" := offHandItem
        ~> jsonEmptyObject
