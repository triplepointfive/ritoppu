module Ritoppu.Model.CreatureType
  ( CreatureType(..)
  ) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Either (Either(..))

data CreatureType
  = RedNagaHatchling
  | RedNaga

instance showCreatureType :: Show CreatureType where
  show = case _ of
    RedNagaHatchling -> "Red naga hatchling"
    RedNaga -> "Red naga"

derive instance eqCreatureType :: Eq CreatureType
derive instance ordCreatureType :: Ord CreatureType

instance decodeJsonCreatureType :: DecodeJson CreatureType where
  decodeJson json = do
    x <- decodeJson json
    creatureType <- x .: "type"
    case creatureType of
      "RedNaga" -> pure RedNaga
      "RedNagaHatchling" -> pure RedNagaHatchling
      _ -> Left ("Unknown CreatureType " <> creatureType)

instance encodeJsonCreatureType :: EncodeJson CreatureType where
  encodeJson = case _ of
    RedNaga ->
      "type" := "RedNaga"
        ~> jsonEmptyObject
    RedNagaHatchling ->
      "type" := "RedNagaHatchling"
        ~> jsonEmptyObject
