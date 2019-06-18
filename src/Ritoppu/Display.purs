module Ritoppu.Display
  ( DisplayTile(..)
  , build
  ) where

import Prelude

import Data.Array (range)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Ritoppu.Model (Stage, Point, playerAt, tileAt)
import Ritoppu.Model.Tile (Tile(..)) as T

data DisplayTile
  = Creature Char DisplayTile
  | Wall
  | Floor
  | Empty

build :: Stage -> Array (Array DisplayTile)
build stage = map
    (\y -> map
        (\x -> toDisplayTile stage { x, y })
        (range 0 (stage.size.x - 1)))
    (range 0 (stage.size.y - 1))

toDisplayTile :: Stage -> Point -> DisplayTile
toDisplayTile stage pos = case tileAt stage pos of
  Just tile | playerAt stage pos -> Creature '@' (stageTileToDisplayTile tile)
  Just tile -> stageTileToDisplayTile tile
  Nothing -> Empty

stageTileToDisplayTile :: T.Tile -> DisplayTile
stageTileToDisplayTile = case _ of
  T.Floor -> Floor
  T.Wall -> Wall
