module Ritoppu.Fov
  ( fov
  , Mask
  ) where

import Prelude

import Control.Monad.State (State, execState, modify_)
import Data.Foldable (for_)
import Data.Int (round, toNumber)
import Data.Map as Map
import Ritoppu.Model (Point)

type Mask = Map.Map Point Boolean

fov :: Number -> Point -> (Point -> Boolean) -> Mask
fov radius origin notSolid = execState calc Map.empty

  where

  doubleRadius = radius * radius

  mark :: Point -> State Mask Unit
  mark point = modify_ (Map.insert point true)

  calc :: State Mask Unit
  calc = do
    mark origin

    when (notSolid origin) $
        for_ deltas $ \{ x, y } -> do
            castLight 0.0 x y 0.0
            castLight x 0.0 0.0 y

  castLight :: Number -> Number -> Number -> Number -> State Mask Unit
  castLight xx xy yx yy = iter
      { row: 1.0
      , start: 1.0
      , end: 0.0
      , distance: 1.0
      , blocked: false
      , newStart: 0.0
      , deltaX: -1.0
      }

    where

    iter params@{ row, start, end, distance, blocked, newStart, deltaX } = case unit of
      _ | start < end
          -> pure unit
      _ | deltaX > 0.0
          -> if blocked || distance > radius - 1.0
            then
              pure unit
            else
              iter params { distance = distance + 1.0, deltaX = -distance - 1.0 }
      _ | currentX > 100 || currentY > 100 || currentX < 0 || currentY < 0 || start < rightSlope
          -> iter nextIter
      _ | end > leftSlope
          -> iter params { distance = distance + 1.0, deltaX = -distance - 1.0 }
      _ -> do
        when
          (doubleDistance deltaX distance <= doubleRadius)
          (mark { x: currentX, y: currentY })

        if blocked
          then
            if notSolid { x: currentX, y: currentY }
            then
              iter nextIter { start = newStart, blocked = false }
            else
              iter nextIter { newStart = rightSlope }
          else
            if not (notSolid { x: currentX, y: currentY }) && distance < radius
            then do
              iter nextIter { newStart = rightSlope, blocked = true }
              iter params { newStart = 0.0, blocked = false, row = distance + 1.0, distance = distance + 1.0, end = leftSlope }
            else
              iter nextIter

      where

      nextIter = params { deltaX = deltaX + 1.0 }

      currentX = round (toNumber origin.x + deltaX * xx - distance * xy)
      currentY = round (toNumber origin.y + deltaX * yx - distance * yy)
      leftSlope = (deltaX - 0.5) / (-distance + 0.5)
      rightSlope = (deltaX + 0.5) / (-distance - 0.5)

doubleDistance :: Number -> Number -> Number
doubleDistance x y = x * x + y * y

deltas :: Array { x :: Number, y :: Number }
deltas =
  [ { x: 1.0, y: 1.0 }
  , { x: 1.0, y: -1.0 }
  , { x: -1.0, y: -1.0 }
  , { x: -1.0, y: 1.0 }
  ]
