module Ritoppu.Mutation.Point
  ( moveTo
  ) where

import Prelude

import Ritoppu.Model (Direction, Point, directionDelta)

moveTo :: Direction -> Point -> Point
moveTo dir pos = pos + directionDelta dir
