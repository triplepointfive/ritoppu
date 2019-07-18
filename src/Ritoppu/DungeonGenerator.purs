module Ritoppu.DungeonGenerator
  ( generator
  ) where

import Prelude

import Data.Array (concatMap, head, index, nub, singleton, (..), (:))
import Data.Foldable (any, foldl, foldr, length)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Traversable (traverse, for)
import Data.Tuple (Tuple(..))
import Ritoppu.Model (CreatureType(..), CreatureRepository, Item(..), Point, Rect, Stage, Tile(..), center, fillRect, initStage, intersect, outerRect, AiStrategy(..), passibleThrough)
import Ritoppu.Mutation (setTile)
import Ritoppu.Random (RandomGenerator, newCreature, newInt, newItem, newPoint, newRect)
import Ritoppu.Utils (nTimes)

maxMonstersPerRoom :: Int
maxMonstersPerRoom = 5

maxItemsPerRoom :: Int
maxItemsPerRoom = 5

creaturesRepository :: CreatureRepository
creaturesRepository v
  | v < 80 =
      { type: RedNagaHatchling
      , stats: { maxHp: 10, hp: 10, defense: 0, power: 3 }
      , turn: 0
      , aiStrategy: BasicAI
      }
  | otherwise =
      { type: RedNaga
      , stats: { maxHp: 16, hp: 16, defense: 1, power: 4 }
      , turn: 0
      , aiStrategy: BasicAI
      }

-- TODO: Reuse CreatureRepository
itemsRepository :: Int -> Item
itemsRepository v
  | v < 70 = HealingPotion
  | v < 80 = FireballScroll
  | v < 90 = ConfusionScroll
  | otherwise = LightningScroll

generator :: Point -> RandomGenerator Stage
generator size = do
  rooms <- traverse (const (newRect size)) (1..30)
  addRooms (initStage size) (filterIntersect rooms)

addRooms :: Stage -> Array Rect -> RandomGenerator Stage
addRooms stage rooms = do
  playerPos <- newPoint a.x a.y b.x b.y
  corridors <- pure $ as Floor $ builtCorridors (map center rooms)

  generateItems =<< generateCreatures (add stage { player { pos = playerPos } } (corridors <> builtRooms))

  where

  add = foldr (\ { pos, tile } -> setTile tile pos)
  { a, b } = fromMaybe ({ a: { x: 0, y: 0 }, b: stage.size }) (head rooms)

  builtRooms = concatMap (as Floor <<< fillRect) rooms

generateCreatures :: Stage -> RandomGenerator Stage
generateCreatures stage = do
  monstersCount <- newInt 0 (maxMonstersPerRoom * 3)

  poses <- nTimes monstersCount (newInt 0 (length availablePoses))

  creaturesList <- for (nub poses) $ \x -> do
    creature <- newCreature creaturesRepository
    pure $ Tuple (fromMaybe { x: 0, y: 0 } (index availablePoses x)) creature

  pure stage { creatures = Map.fromFoldable creaturesList }

  where

  availablePoses :: Array Point
  availablePoses
    = Set.toUnfoldable
    $ Set.delete stage.player.pos
    $ Map.keys
    $ Map.filter passibleThrough stage.tiles

-- TODO: Remove duplicity
-- EXTRA: Pass real # of rooms
generateItems :: Stage -> RandomGenerator Stage
generateItems stage = do
  monstersCount <- newInt 0 (maxItemsPerRoom * 3)

  poses <- nTimes monstersCount (newInt 0 (length availablePoses))

  itemsList <- for (nub poses) $ \x -> do
    item <- singleton <$> newItem itemsRepository
    pure $ Tuple (fromMaybe { x: 0, y: 0 } (index availablePoses x)) item

  pure stage { items = Map.fromFoldable itemsList }

  where

  availablePoses :: Array Point
  availablePoses
    = Set.toUnfoldable
    $ Map.keys
    $ Map.filter passibleThrough stage.tiles

builtCorridors :: Array Point -> Array Point
builtCorridors = _.tiles <<< foldl connect { last: Nothing, tiles: [] }

  where

  connect { last, tiles } new@{ x: x2, y: y2 } = case last of
    Just point@{ x: x1, y: y1 } ->
      { last: Just new
      , tiles: buildHLine x1 x2 y1
        <> buildVLine y1 y2 x2
        <> tiles
      }
    Nothing -> { last: Just new, tiles }

filterIntersect :: Array Rect -> Array Rect
filterIntersect = foldl checkRoom []

  where

  checkRoom :: Array Rect -> Rect -> Array Rect
  checkRoom rects rect
    | any (intersect (outerRect rect)) rects = rects
    | otherwise = rect : rects

type GenTile = { pos :: Point, tile :: Tile }

as :: Tile -> Array Point -> Array GenTile
as tile = map { pos: _, tile }

buildHLine :: Int -> Int -> Int -> Array Point
buildHLine x1 x2 y = map (\x -> { x, y }) (x1..x2)

buildVLine :: Int -> Int -> Int -> Array Point
buildVLine y1 y2 x = map (\y -> { x, y }) (y1..y2)
