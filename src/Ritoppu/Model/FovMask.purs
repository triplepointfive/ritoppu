module Ritoppu.Model.FovMask
  ( FovMask(..)
  , initFovMask
  , isVisibleTile
  ) where

import Data.Map as Map
import Ritoppu.Model.Point (Point)

type FovMask = Map.Map Point Boolean

initFovMask :: FovMask
initFovMask = Map.empty

isVisibleTile :: FovMask -> Point -> Boolean
isVisibleTile mask pos = Map.member pos mask
