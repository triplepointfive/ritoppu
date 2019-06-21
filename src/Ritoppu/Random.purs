module Ritoppu.Random
  ( RandomGenerator
  , module Random.PseudoRandom
  , runGenerator
  , newPoint
  , newInt
  , newRect
  ) where

import Prelude

import Control.Monad.State (State, evalState, get, put)
import Ritoppu.Model (Point, Rect)
import Random.PseudoRandom (Seed, randomR, randomSeed)

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

newRect :: Point -> RandomGenerator Rect
newRect { x: width, y: height } = do
  { x: w, y: h } <- newPoint 6 6 10 10
  { x, y } <- newPoint 1 1 (width - w - 1) (height - h - 1)
  pure { a: { x, y }, b: { x: x + w, y: y + h } }
