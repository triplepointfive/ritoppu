module Ritoppu.DungeonGenerator
  ( generator
  ) where

import Prelude

import Data.Array (concatMap, head, (..), (:))
import Data.Foldable (any, foldl, foldr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Ritoppu.Model (Tile(..), Stage, Point, initStage, Rect, intersect, center, outerRect, fillRect)
import Ritoppu.Mutation (setTile)
import Ritoppu.Random (RandomGenerator, newPoint, newRect)

generator :: Point -> RandomGenerator Stage
generator size = do
  rooms <- traverse (const (newRect size)) (1..30)
  addRooms (initStage size) (filterIntersect rooms)

addRooms :: Stage -> Array Rect -> RandomGenerator Stage
addRooms stage rooms = do
  playerPos <- newPoint a.x a.y b.x b.y
  corridors <- pure $ as Floor $ builtCorridors (map center rooms)

  pure (add stage { player = { pos: playerPos } } (corridors <> builtRooms))

  where

  add = foldr (\ { pos, tile } -> setTile tile pos)
  { a, b } = fromMaybe ({ a: { x: 0, y: 0 }, b: stage.size }) (head rooms)

  builtRooms = concatMap (as Floor <<< fillRect) rooms

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
