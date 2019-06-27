module Ritoppu.Model.Stage
  ( Stage
  , creatureAt
  , playerAt
  , tileAt
  , availableToMoveTo
  , initStage
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Set as Set
import Ritoppu.Model.FovMask (FovMask)
import Ritoppu.Model.Point (Point)
import Ritoppu.Model.Tile (Tile, passibleThrough)

type Stage =
  { player :: { pos :: Point }
  , tiles :: Map.Map Point Tile
  , size :: { x :: Int, y :: Int }
  , seen :: Set.Set Point -- TODO: Should remember which item was there
  , fovMask :: FovMask
  }

availableToMoveTo :: Stage -> Point -> Boolean
availableToMoveTo stage pos =
  maybe false passibleThrough (tileAt stage pos)

playerAt :: Stage -> Point -> Boolean
playerAt stage pos = stage.player.pos == pos

creatureAt :: Stage -> Point -> Boolean
creatureAt stage pos = false

tileAt :: Stage -> Point -> Maybe Tile
tileAt stage pos = Map.lookup pos stage.tiles

initStage :: Point -> Stage
initStage size =
  { player: { pos: { x: 0, y: 0 } }
  , tiles: Map.empty
  , size
  , seen: Set.empty
  , fovMask: Map.empty
  }
