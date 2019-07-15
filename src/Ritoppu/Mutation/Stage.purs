module Ritoppu.Mutation.Stage
  ( setTile
  , updateFov
  , addCreature
  , removeCreature
  , moveCreature
  , updateCreature
  , addItem
  , removeItem
  ) where

import Prelude

import Data.Array (cons, delete)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Ritoppu.Model (Creature, Item, Point, Stage, Tile, availableToMoveTo)
import Ritoppu.Mutation.FovMask (rebuildFov)

setTile :: Tile -> Point -> Stage  -> Stage
setTile tile pos stage = stage { tiles = Map.insert pos tile stage.tiles }

updateFov :: Stage -> Stage
updateFov stage = stage
  { fovMask = rebuildFov 10 stage.player.pos (availableToMoveTo stage) stage.fovMask
  }

addItem :: Point -> Item -> Stage -> Stage
addItem pos item stage
  = stage { items = Map.alter (Just <<< maybe [item] (cons item)) pos stage.items }

removeItem :: Point -> Item -> Stage -> Stage
removeItem pos item stage
  = stage { items = Map.alter removeItemInCell pos stage.items }

  where

  removeItemInCell = case _ of
    Nothing -> Nothing
    Just xs -> case delete item xs of
      [] -> Nothing
      ys -> Just ys

addCreature :: Point -> Creature -> Stage -> Stage
addCreature pos creature stage =
  stage { creatures = Map.insert pos creature stage.creatures }

removeCreature :: Point -> Stage -> Stage
removeCreature pos stage =
  stage { creatures = Map.delete pos stage.creatures }

moveCreature :: Point -> Point -> Creature -> Stage -> Stage
moveCreature from dest creature = addCreature dest creature <<< removeCreature from

updateCreature :: Point -> (Creature -> Creature) -> Stage -> Stage
updateCreature pos f stage
  = stage { creatures = Map.update (Just <<< f) pos stage.creatures }
