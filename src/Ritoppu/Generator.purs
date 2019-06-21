module Ritoppu.Generator where

import Prelude

import Data.Array (range)
import Data.Foldable (foldr)
import Ritoppu.Model (Tile(..), Stage, Point)
import Ritoppu.Mutation (setTile)

generate :: Int -> Stage -> Stage
generate r stage =
  foldr
    (\ { pos, tile } -> setTile tile pos)
    stage
    (as Wall (circle 0 r (1 - 2 * r)) <> as Floor (solidCircle 0 r (1 - 2 * r)))

as :: Tile -> Array Point -> Array { pos :: Point, tile :: Tile }
as tile = map { pos: _, tile }

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

  top = map ({ x: _, y: 15 + y }) $ range (15 - x + 1) (15 + x - 1)
  bottom = map ({ x: _, y: 15 - y }) $ range (15 - x + 1) (15 + x - 1)

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
    [ { x: 15 + x, y: 15 + y}
    , { x: 15 + x, y: 15 - y}
    , { x: 15 - x, y: 15 + y}
    , { x: 15 - x, y: 15 - y}
    ]

  extraTiles =
    [ { x: 15 + 1 + x, y: 15 + y}
    , { x: 15 + 1 + x, y: 15 - y}
    , { x: 15 - x - 1, y: 15 + y}
    , { x: 15 - x - 1, y: 15 - y}
    ]

