module Ritoppu.Model.Creature
  ( AiStrategy(..)
  , Creature
  , creatureName
  ) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Either (Either(..))
import Ritoppu.Model.CreatureType (CreatureType)
import Ritoppu.Model.Stats (Stats)

data AiStrategy
  = BasicAI
  | ConfusedAI Int AiStrategy

instance decodeJsonAiStrategy :: DecodeJson AiStrategy where
  decodeJson json = do
    x <- decodeJson json
    strategy <- x .: "type"
    case strategy of
      "basic" -> pure BasicAI
      "confused" -> do
        turns <- x .: "turns"
        prevAiStrategy <- x .: "prevAiStrategy"
        pure (ConfusedAI turns prevAiStrategy)
      _ -> Left ("Unknown AiStrategy type " <> strategy)

instance encodeJsonAiStrategy :: EncodeJson AiStrategy where
  encodeJson = case _ of
    BasicAI ->
      "type" := "basic"
        ~> jsonEmptyObject
    ConfusedAI turns prevAiStrategy ->
      "type" := "confused"
        ~> "turns" := turns
        ~> "prevAiStrategy" := prevAiStrategy
        ~> jsonEmptyObject

type Creature =
  { type :: CreatureType
  , stats :: Stats
  , turn :: Int
  , aiStrategy :: AiStrategy
  }

creatureName :: Creature -> String
creatureName creature = show creature.type
