module Ritoppu.Model.Stage
  ( Stage
  , anybodyAt
  , creatureAt
  , playerAt
  , tileAt
  , availableToMoveTo
  , initStage
  , itemAt
  ) where

import Prelude

import Data.Array (head)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Set as Set
import Ritoppu.Model.Creature (Creature)
import Ritoppu.Model.FovMask (FovMask, initFovMask)
import Ritoppu.Model.Item (Item)
import Ritoppu.Model.Player (Player, initPlayer)
import Ritoppu.Model.Point (Point)
import Ritoppu.Model.Tile (Tile(..), passibleThrough)

type Stage =
  { player :: Player
  , tiles :: Map.Map Point Tile
  , size :: { x :: Int, y :: Int }
  , seen :: Set.Set Point -- EXTRA: Should remember which item was there
  , items :: Map.Map Point (Array Item)
  , fovMask :: FovMask
  , creatures :: Map.Map Point Creature
  }

itemAt :: Stage -> Point -> Maybe Item
itemAt stage pos = head $ fromMaybe [] (Map.lookup pos stage.items)

anybodyAt :: Stage -> Point -> Boolean
anybodyAt stage pos = Map.member pos stage.creatures

creatureAt :: Stage -> Point -> Maybe Creature
creatureAt stage pos = Map.lookup pos stage.creatures

availableToMoveTo :: Stage -> Point -> Boolean
availableToMoveTo stage pos = passibleThrough (tileAt stage pos)

playerAt :: Stage -> Point -> Boolean
playerAt stage pos = stage.player.pos == pos

tileAt :: Stage -> Point -> Tile
tileAt stage pos = fromMaybe Wall (Map.lookup pos stage.tiles)

initStage :: Point -> Stage
initStage size =
  { player: initPlayer
  , tiles: Map.empty
  , size
  , seen: Set.empty
  , items: Map.empty
  , fovMask: initFovMask
  , creatures: Map.empty
  }
