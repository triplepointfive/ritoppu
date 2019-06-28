module Ritoppu.Random
  ( RandomGenerator
  , module Random.PseudoRandom
  , runGenerator
  , newPoint
  , newPointInRect
  , newInt
  , newRect
  ) where

import Prelude

import Control.Monad.State (State, evalState, get, put)
import Data.Array (length, null, index, (:))
import Data.Maybe (Maybe(..))
import Random.PseudoRandom (Seed, randomR, randomSeed)
import Ritoppu.Model (Point, Rect)

type RandomGenerator a = State Seed a

runGenerator :: forall a. Seed -> RandomGenerator a -> a
runGenerator seed f = evalState f seed

newInt :: Int -> Int -> RandomGenerator Int
newInt from to = do
  seed <- get
  let { newVal , newSeed } = randomR from to seed
  put newSeed
  pure newVal

newPoint :: Int -> Int -> Int -> Int -> RandomGenerator Point
newPoint fromX fromY toX toY = do
  x <- newInt fromX toX
  y <- newInt fromY toY
  pure { x, y }

samples :: forall a. Int ->  Array a -> RandomGenerator (Array a)
samples n xs
  | null xs = pure []
  | n > 0 = do
      i <- newInt 0 (length xs)
      case index xs i of
        Just v -> do
          rest <- samples (n - 1) xs
          pure (v : rest)
        Nothing -> do
          pure []
  | otherwise = pure []

newPointInRect :: Rect -> RandomGenerator Point
newPointInRect { a: { x: fromX, y: fromY }, b: { x: toX, y: toY } } = do
  x <- newInt fromX toX
  y <- newInt fromY toY
  pure { x, y }

newRect :: Point -> RandomGenerator Rect
newRect { x: width, y: height } = do
  { x: w, y: h } <- newPoint 6 6 10 10
  { x, y } <- newPoint 1 1 (width - w - 1) (height - h - 1)
  pure { a: { x, y }, b: { x: x + w, y: y + h } }
