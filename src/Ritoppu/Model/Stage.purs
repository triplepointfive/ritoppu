module Ritoppu.Model.Stage
  ( Stage
  , creatureAt
  , playerAt
  , tileAt
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe)

import Ritoppu.Model.Point (Point)
import Ritoppu.Model.Tile (Tile)

type Stage =
  { player :: { pos :: Point }
  , tiles :: Map.Map Point Tile
  , size :: { x :: Int, y :: Int }
  }

playerAt :: Stage -> Point -> Boolean
playerAt stage pos = stage.player.pos == pos

creatureAt :: Stage -> Point -> Boolean
creatureAt stage pos = false

tileAt :: Stage -> Point -> Maybe Tile
tileAt stage pos = Map.lookup pos stage.tiles
