module Ritoppu.Mutation.FovMask
  ( rebuildFov
  ) where

import Prelude

import Data.Array ((..))
import Control.Monad.State (State, execState, modify_)
import Data.Foldable (for_, foldM)
import Data.Int (toNumber)
import Data.Map as Map
import Ritoppu.Model (Point, FovMask)

rebuildFov :: Int -> Point -> (Point -> Boolean) -> FovMask
rebuildFov radius origin notSolid = execState calc Map.empty

  where

  doubleRadius = radius * radius

  mark :: Point -> State FovMask Unit
  mark point = modify_ (Map.insert point true)

  calc :: State FovMask Unit
  calc = do
    mark origin

    when (notSolid origin) $
        for_
            [ { x: 1, y: 1 }
            , { x: 1, y: -1 }
            , { x: -1, y: -1 }
            , { x: -1, y: 1 }
            ]
            \{ x, y } -> do
                castLight 0 x y 0
                castLight x 0 0 y

  castLight :: Int -> Int -> Int -> Int -> State FovMask Unit
  castLight xx xy yx yy = beam 1 1.0 0.0

    where

    beam row start end = when (start > end) do
      void $ foldM
          withDistance
          { newStart: 0.0, blocked: false, start, end, stop: false }
          (row .. radius)

    withDistance meta distance
      | meta.blocked = pure meta
      | otherwise = foldM (withDeltaX distance) (meta { stop = false }) (distance .. 0)

    withDeltaX deltaY meta@{ start, end, stop, blocked, newStart } deltaX = case unit of
      _ | stop || current.x < 0 || current.y < 0 || current.x > 100 || current.y > 100 || start <= rightSlope
          -> pure meta
      _ | end > leftSlope -> pure meta { stop = true }
      _ -> do
        when (doubleDistance deltaX deltaY <= doubleRadius) do
            mark current

        case notSolid current of
            true | blocked -> pure meta { blocked = false, start = newStart }
            false | blocked -> pure meta { newStart = rightSlope }
            false | deltaY < radius -> do
                beam (deltaY + 1) start leftSlope
                pure meta { blocked = true, newStart = rightSlope }
            _ -> pure meta

      where

      current =
          { x: origin.x + deltaX * xx + deltaY * xy
          , y: origin.y + deltaX * yx + deltaY * yy
          }

      leftSlope = (toNumber deltaX + 0.5) / (toNumber deltaY - 0.5)
      rightSlope = (toNumber deltaX - 0.5) / (toNumber deltaY + 0.5)

  doubleDistance :: Int -> Int -> Int
  doubleDistance x y = x * x + y * y
