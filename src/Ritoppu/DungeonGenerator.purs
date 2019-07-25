module Ritoppu.DungeonGenerator
  ( generator
  ) where

import Prelude

import Data.Array (concatMap, find, head, index, last, nub, singleton, (..), (:))
import Data.Foldable (any, foldl, foldr, length)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Traversable (traverse, for)
import Data.Tuple (Tuple(..))
import Ritoppu.Model (AiStrategy(..), Creature, CreatureType(..), Item(..), Point, Rect, Repository, Stage, Tile(..), center, fillRect, initRepository, initStage, intersect, outerRect, passibleThrough)
import Ritoppu.Mutation (addToRepository, setTile)
import Ritoppu.Random (RandomGenerator, newFromRepository, newInt, newPoint, newRect)
import Ritoppu.Utils (nTimes)

creaturesRepository :: Int -> Repository Creature
creaturesRepository dungeonLevel
  = addToRepository (pickInDict [{ level: 7, value: 60 }, { level: 5, value: 30 }, { level: 3, value: 15 }] dungeonLevel) redNaga
  $ initRepository 80 redNagaHatchling

  where

  redNagaHatchling =
    { type: RedNagaHatchling
    , stats: { maxHp: 10, hp: 10, defense: 0, power: 3 }
    , turn: 0
    , aiStrategy: BasicAI
    , xp: 35
    }

  redNaga =
    { type: RedNaga
    , stats: { maxHp: 16, hp: 16, defense: 1, power: 4 }
    , turn: 0
    , aiStrategy: BasicAI
    , xp: 100
    }

itemsRepository :: Int -> Repository Item
itemsRepository dungeonLevel
  = addToRepository (pickInDict [{ level: 4, value: 25 }] dungeonLevel) LightningScroll
  $ addToRepository (pickInDict [{ level: 6, value: 25 }] dungeonLevel) FireballScroll
  $ addToRepository (pickInDict [{ level: 2, value: 10 }] dungeonLevel) ConfusionScroll
  $ initRepository 35 HealingPotion

generator :: Int -> Point -> RandomGenerator Stage
generator dungeonLevel size = do
  rooms <- traverse (const (newRect size)) (1..30)
  addRooms dungeonLevel (initStage size) (filterIntersect rooms)

addRooms :: Int -> Stage -> Array Rect -> RandomGenerator Stage
addRooms dungeonLevel stage rooms = do
  -- EXTRA: Validate never has intercalation
  playerPos <- newPoint a.x a.y b.x b.y
  stairsDownPos <- newPoint lastRoom.a.x lastRoom.a.y lastRoom.b.x lastRoom.b.y

  generateCreatures dungeonLevel (length rooms) (stageWithFloors playerPos stairsDownPos)
    >>= generateItems dungeonLevel (length rooms)

  where

  builtRooms = concatMap (as Floor <<< fillRect) rooms
  corridors = as Floor $ builtCorridors (map center rooms)
  stairs playerPos stairsDownPos = as StairsUp [playerPos] <> as StairsDown [stairsDownPos]

  stageWithFloors playerPos stairsDownPos = add
    stage { player { pos = playerPos } }
    ((stairs playerPos stairsDownPos) <> corridors <> builtRooms)

  add = foldr (\ { pos, tile } -> setTile tile pos)
  { a, b } = fromMaybe ({ a: { x: 0, y: 0 }, b: stage.size }) (head rooms)
  lastRoom = fromMaybe ({ a: { x: 0, y: 0 }, b: stage.size }) (last rooms)

generateCreatures :: Int -> Int -> Stage -> RandomGenerator Stage
generateCreatures dungeonLevel roomsCount stage = do
  monstersCount <- newInt 0 (maxMonstersPerRoom * roomsCount)

  poses <- nTimes monstersCount (newInt 0 (length availablePoses))

  creaturesList <- for (nub poses) $ \x -> do
    creature <- newFromRepository (creaturesRepository dungeonLevel)
    pure $ Tuple (fromMaybe { x: 0, y: 0 } (index availablePoses x)) creature

  pure stage { creatures = Map.fromFoldable creaturesList }

  where

  availablePoses :: Array Point
  availablePoses
    = Set.toUnfoldable
    $ Set.delete stage.player.pos
    $ Map.keys
    $ Map.filter passibleThrough stage.tiles

  maxMonstersPerRoom :: Int
  maxMonstersPerRoom =
    pickInDict
      [{ level: 6, value: 5 }, { level: 4, value: 3 }, { level: 1, value: 2 }]
      dungeonLevel

-- TODO: Remove duplicity
-- EXTRA: Pass real # of rooms
generateItems :: Int -> Int -> Stage -> RandomGenerator Stage
generateItems dungeonLevel roomsCount stage = do
  monstersCount <- newInt 0 (maxItemsPerRoom * roomsCount)

  poses <- nTimes monstersCount (newInt 0 (length availablePoses))

  itemsList <- for (nub poses) $ \x -> do
    item <- singleton <$> newFromRepository (itemsRepository dungeonLevel)
    pure $ Tuple (fromMaybe { x: 0, y: 0 } (index availablePoses x)) item

  pure stage { items = Map.fromFoldable itemsList }

  where

  availablePoses :: Array Point
  availablePoses
    = Set.toUnfoldable
    $ Map.keys
    $ Map.filter passibleThrough stage.tiles

  maxItemsPerRoom :: Int
  maxItemsPerRoom =
    pickInDict
      [{ level: 4, value: 2 }, { level: 1, value: 1 }]
      dungeonLevel

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

pickInDict :: Array { level :: Int, value :: Int } -> Int -> Int
pickInDict list dungeonLevel
  = maybe 0 (_.value)
  $ find (\{ level } -> dungeonLevel >= level) list
