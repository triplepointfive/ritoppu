module Ritoppu.Utils where

import Prelude

import Data.Array ((:))

nTimes :: forall m a. Monad m => Int -> (m a) -> m (Array a)
nTimes i f
  | i > 0 = do
      x <- f
      xs <- nTimes (i - 1) f
      pure (x : xs)
  | otherwise = pure []
