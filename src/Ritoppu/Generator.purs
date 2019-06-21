module Ritoppu.Generator
  ( generate
  ) where

import Prelude

import Data.Array (range, take, head, (:))
import Data.Foldable (foldr, foldl, find, elem)
import Data.Int (floor, ceil, toNumber)
import Data.Ord (abs)
import Data.Maybe (Maybe(..))
import Math (cos, pi, sin)
import Ritoppu.Model (Tile(..), Stage, Point)
import Ritoppu.Mutation (setTile)

generate :: Int -> Stage -> Stage
generate r stage =
  foldr
    (\ { pos: { x, y }, tile } -> setTile tile { x: x + 15, y: y + 15 })
    stage
    (build 15 <> as Floor (solidCircle 0 r (1 - 2 * r)))

type GenTile = { pos :: Point, tile :: Tile }

as :: Tile -> Array Point -> Array GenTile
as tile = map { pos: _, tile }

angles :: Array Int
angles = [4, 5, 14, 19, 26, 29, 36, 38, 39, 41, 42, 47, 49, 64, 66, 72, 73, 75, 78, 81, 82, 90, 102, 104, 107, 110, 114, 115, 117, 120, 127, 128, 132, 142, 143, 152, 155, 156, 157, 160, 162, 163, 166, 169, 172, 175, 176, 179, 182, 183, 185, 189, 205, 209, 217, 219, 226, 232, 241, 243, 246, 248, 251, 252, 261, 270, 271, 275, 278, 284, 287, 291, 298, 301, 305, 306, 312, 316, 317, 323, 324, 328, 334, 335, 337, 338, 341, 353, 355, 356, 359]

build r = as Wall (circlePoints walls)
  where

  walls = circle 0 r (1 - 2 * r)

circlePoints :: Array Point -> Array Point
circlePoints walls = foldl beam [] angles

  where

  beam :: Array Point -> Int -> Array Point
  beam points angle = case ray angle 16 of
    Just point -> case head points of
      Just { x, y } | abs (point.x - x) > 3 && abs (point.y - y) > 3 -> point : points
      Just _ -> points
      Nothing -> [point]
    _ -> points

  ray :: Int -> Int -> Maybe Point
  ray angle r =
    find
      (\pos -> elem pos walls)
      [ { x: floor x, y: ceil y }
      , { x: floor x, y: floor y }
      , { x: ceil x, y: floor y }
      , { x: ceil x, y: ceil y }
      ]

    where

    x = toNumber 15 * cos (toNumber angle / 180.0 * pi)
    y = toNumber 15 * sin (toNumber angle / 180.0 * pi)

solidCircle :: Int -> Int -> Int -> Array Point
solidCircle x y delta = case unit of
  _ | y < 0 -> []
  _ | delta < 0 && error <= 0
    -> tiles <> solidCircle (x+1) y (delta + 2 * (1 + x) + 1)
  _ | delta > 0 && error > 0
    -> tiles <> solidCircle x (y-1) (delta - 2 * (y - 1) + 1)
  _ -> tiles <> solidCircle (x+1) (y-1) (delta + 2 * (1 + x - y))

  where

  error = 2 * (delta + y) - 1

  tiles = top <> bottom

  top = map ({ x: _, y: y }) $ range (1 - x) (x - 1)
  bottom = map ({ x: _, y: -y }) $ range (1 - x) (x - 1)

circle :: Int -> Int -> Int -> Array Point
circle x y delta = case unit of
  _ | y < 0 -> []
  _ | delta < 0 && error <= 0
    -> tiles <> circle (x+1) y (delta + 2 * (1 + x) + 1)
  _ | delta > 0 && error > 0
    -> tiles <> circle x (y-1) (delta - 2 * (y - 1) + 1)
  _ -> extraTiles <> tiles <> circle (x+1) (y-1) (delta + 2 * (1 + x - y))

  where

  error = 2 * (delta + y) - 1

  tiles =
    [ { x: x, y: y }
    , { x: x, y: -y }
    , { x: -x, y: y }
    , { x: -x, y: -y }
    ]

  extraTiles =
    [ { x: 1 + x, y: y }
    , { x: 1 + x, y: -y }
    , { x: -x - 1, y: y }
    , { x: -x - 1, y: -y }
    ]

