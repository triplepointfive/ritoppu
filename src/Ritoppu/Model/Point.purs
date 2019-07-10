module Ritoppu.Model.Point
  ( Point
  , adjustPoints
  , isNextTo
  ) where

import Prelude

import Data.Ord (abs)

type Point =
  { x :: Int
  , y :: Int
  }

adjustPoints :: Point -> Array Point
adjustPoints { x, y } =
  [ { x: x + 1, y: y + 1 }
  , { x: x + 1, y: y - 1 }
  , { x: x - 1, y: y + 1 }
  , { x: x - 1, y: y - 1 }
  , { x: x + 1, y: y }
  , { x: x, y: y + 1 }
  , { x: x, y: y - 1 }
  , { x: x - 1, y: y }
  ]

isNextTo :: Point -> Point -> Boolean
isNextTo { x: x1, y: y1 } { x: x2, y: y2 }
  = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1
