module Ritoppu.Model.Direction
  ( Direction(..)
  , directionDelta
  ) where

import Prelude

import Ritoppu.Model.Point (Point)

data Direction
  = N
  | NE
  | E
  | SE
  | S
  | SW
  | W
  | NW

directionDelta :: Direction -> Point
directionDelta = case _ of
  N -> { x: 0, y: -1 }
  NE -> { x: 1, y: -1 }
  E -> { x: 1, y: 0 }
  SE -> { x: 1, y: 1 }
  S -> { x: 0, y: 1 }
  SW -> { x: -1, y: 1 }
  W -> { x: -1, y: 0 }
  NW -> { x: -1, y: -1 }
