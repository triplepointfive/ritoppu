module Ritoppu.Model.Tile
  ( Tile(..)
  , passibleThrough
  , seeableThrough
  ) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Either (Either(..))

data Tile
  = Floor
  | Wall
  | StairsUp
  | StairsDown

passibleThrough :: Tile -> Boolean
passibleThrough = case _ of
  Floor -> true
  Wall -> false
  StairsDown -> true
  StairsUp -> true

seeableThrough :: Tile -> Boolean
seeableThrough = case _ of
  Floor -> true
  Wall -> false
  StairsDown -> true
  StairsUp -> true

instance showTile :: Show Tile where
  show = case _ of
    Floor -> "F"
    Wall -> "W"
    StairsUp -> "<"
    StairsDown -> "<"

instance decodeJsonTile :: DecodeJson Tile where
  decodeJson json = do
    x <- decodeJson json
    tile <- x .: "tile"
    case tile of
      "Wall" -> pure Wall
      "Floor" -> pure Floor
      "StairsDown" -> pure StairsDown
      "StairsUp" -> pure StairsUp
      _ -> Left ("Unknown Tile " <> tile)

instance encodeJsonTile :: EncodeJson Tile where
  encodeJson = case _ of
    Floor ->
      "tile" := "Floor"
        ~> jsonEmptyObject
    Wall ->
      "tile" := "Wall"
        ~> jsonEmptyObject
    StairsDown ->
      "tile" := "StairsDown"
        ~> jsonEmptyObject
    StairsUp ->
      "tile" := "StairsUp"
        ~> jsonEmptyObject
