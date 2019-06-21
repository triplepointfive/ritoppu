module Ritoppu.Model.Rect
  ( Rect
  , center
  , fillRect
  , innerRect
  , intersect
  , outerRect
  ) where

import Prelude

import Data.Array ((..))
import Ritoppu.Model.Point (Point)

type Rect = { a :: Point, b :: Point }

fillRect :: Rect -> Array Point
fillRect { a, b } = do
  x <- a.x .. b.x
  y <- a.y .. b.y
  pure { x, y }

innerRect :: Rect -> Rect
innerRect { a, b } =
  { a: { x: a.x + 1, y: a.y + 1 }
  , b: { x: b.x - 1, y: b.y - 1 }
  }

outerRect :: Rect -> Rect
outerRect { a, b } =
  { a: { x: a.x - 1, y: a.y - 1 }
  , b: { x: b.x + 1, y: b.y + 1 }
  }

center :: Rect -> Point
center { a, b } = { x: (a.x + b.x) / 2, y: (a.y + b.y) / 2 }

intersect :: Rect -> Rect -> Boolean
intersect { a: a1, b: b1 } { a: a2, b: b2 } =
  not (a1.x > b2.x || a2.x > b1.x || a1.y > b2.y || a2.y > b1.y)
