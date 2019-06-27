module Ritoppu.Model.FovMask
  ( FovMask(..)
  , initFovMask
  , isVisibleTile
  , isSeenTile
  ) where

import Data.Map as Map
import Data.Set as Set
import Ritoppu.Model.Point (Point)

type FovMask =
  { visible :: Map.Map Point Boolean
  , seen :: Set.Set Point
  }

initFovMask :: FovMask
initFovMask = { visible: Map.empty, seen: Set.empty }

isVisibleTile :: FovMask -> Point -> Boolean
isVisibleTile { visible } pos = Map.member pos visible

isSeenTile :: FovMask -> Point -> Boolean
isSeenTile { seen } pos = Set.member pos seen
