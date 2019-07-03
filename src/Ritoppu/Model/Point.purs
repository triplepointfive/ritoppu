module Ritoppu.Model.Point
  ( Point
  , adjustPoints
  ) where

import Prelude

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
