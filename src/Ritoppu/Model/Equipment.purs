module Ritoppu.Model.Equipment
  ( MainHandItem(..)
  , OffHandItem(..)
  , Equipment
  , initEquipment
  ) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

data MainHandItem
  = Dagger
  | Sword

derive instance eqMainHandItem :: Eq MainHandItem
derive instance ordMainHandItem :: Ord MainHandItem

instance showMainHandItem :: Show MainHandItem where
  show = case _ of
    Dagger -> "Dagger"
    Sword -> "Sword"

instance decodeJsonMainHandItem :: DecodeJson MainHandItem where
  decodeJson json = do
    x <- decodeJson json
    item <- x .: "item"
    case item of
      "Dagger" -> pure Dagger
      "Sword" -> pure Sword
      _ -> Left ("Unknown MainHandItem " <> item)

instance encodeJsonMainHandItem :: EncodeJson MainHandItem where
  encodeJson = case _ of
    Dagger ->
      "item" := "Dagger"
        ~> jsonEmptyObject
    Sword ->
      "item" := "Sword"
        ~> jsonEmptyObject

data OffHandItem
  = Shield

derive instance eqOffHandItem :: Eq OffHandItem
derive instance ordOffHandItem :: Ord OffHandItem

instance showOffHandItem :: Show OffHandItem where
  show = case _ of
    Shield -> "Shield"

instance decodeJsonOffHandItem :: DecodeJson OffHandItem where
  decodeJson json = do
    x <- decodeJson json
    item <- x .: "item"
    case item of
      "Shield" -> pure Shield
      _ -> Left ("Unknown OffHandItem " <> item)

instance encodeJsonOffHandItem :: EncodeJson OffHandItem where
  encodeJson = case _ of
    Shield ->
      "item" := "Shield"
        ~> jsonEmptyObject

type Equipment =
  { mainHand :: Maybe MainHandItem
  , offHand :: Maybe OffHandItem
  }

initEquipment :: Equipment
initEquipment =
  { mainHand: Nothing
  , offHand: Nothing
  }