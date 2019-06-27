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
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Ritoppu.Model.FovMask (FovMask, initFovMask)
import Ritoppu.Model.Point (Point)
import Ritoppu.Model.Tile (Tile(..), passibleThrough)

type Stage =
  { player :: { pos :: Point }
  , tiles :: Map.Map Point Tile
  , size :: { x :: Int, y :: Int }
  , seen :: Set.Set Point -- TODO: Should remember which item was there
  , fovMask :: FovMask
  }

availableToMoveTo :: Stage -> Point -> Boolean
availableToMoveTo stage pos = passibleThrough (tileAt stage pos)

playerAt :: Stage -> Point -> Boolean
playerAt stage pos = stage.player.pos == pos

creatureAt :: Stage -> Point -> Boolean
creatureAt stage pos = false

tileAt :: Stage -> Point -> Tile
tileAt stage pos = fromMaybe Wall (Map.lookup pos stage.tiles)

initStage :: Point -> Stage
initStage size =
  { player: { pos: { x: 0, y: 0 } }
  , tiles: Map.empty
  , size
  , seen: Set.empty
  , fovMask: initFovMask
  }
