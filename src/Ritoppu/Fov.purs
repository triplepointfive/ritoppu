module Ritoppu.Fov
  ( fov
  ) where

import Prelude

import Control.Monad.State (evalState, execState, modify_, State)
import Data.Foldable (foldl, for_)
import Data.Int (round, toNumber)
import Data.Map as Map
import Halogen (HalogenF(..))
import Ritoppu.Model (Point)

type Mask = Map.Map Point Boolean

fov :: Int -> Point -> (Point -> Boolean) -> Mask
fov radius origin notSolid = execState calc Map.empty

  where

  doubleRadius = toNumber radius * toNumber radius

  mark :: Point -> State Mask Unit
  mark point = modify_ (Map.insert point true)

  calc :: State Mask Unit
  calc = do
    mark origin

    when (notSolid origin) $
        for_ deltas $ \{ x, y } -> do
          castLight 0.0 x y 0.0
          castLight x 0.0 0.0 y

    pure unit

  castLight :: Number -> Number -> Number -> Number -> State Mask Unit
  castLight xx xy yx yy = iter 1.0 1.0 0.0 row false 0.0

    where

    iter row start end distance blocked newStart
      | start < end = pure unit




    | otherwise = withDistance

    where

    withDistance :: Number -> Boolean -> Number -> State Mask Unit
    withDistance
      | blocked || distance > toNumber radius = pure unit
      | otherwise = withDelta (-distance) (-distance)

    withDelta :: Number -> Number -> State Mask Unit
    withDelta deltaX deltaY = case unit of
      _ | deltaX > 0.0 -> pure unit
      _ | start < rightSlope -> withDelta (deltaX + 1.0) deltaY
      -- TODO: Required below? currentX >= this.width || currentY >= this.height
      _ | currentX < 0 || currentY < 0 -> withDelta (deltaX + 1.0) deltaY
      _ | end > leftSlope -> pure unit
      _ -> do
        when
          (doubleDistance deltaX deltaY <= doubleRadius)
          (mark { x: currentX, y: currentY })
          -- 1 - (doubleDistance deltaX deltaY / doubleRadius)

      where

      currentX = round (toNumber origin.x + deltaX * xx + deltaY * xy)
      currentY = round (toNumber origin.y + deltaX * yx + deltaY * yy)
      leftSlope = (deltaX - 0.5) / (deltaY + 0.5)
      rightSlope = (deltaX + 0.5) / (deltaY - 0.5)


doubleDistance :: Number -> Number -> Number
doubleDistance x y = x * x + y * y

deltas =
  [ { x: 1.0, y: 1.0 }
  , { x: 1.0, y: -1.0 }
  , { x: -1.0, y: -1.0 }
  , { x: -1.0, y: 1.0 }
  ]
