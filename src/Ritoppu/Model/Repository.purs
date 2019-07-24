module Ritoppu.Model.Repository
  ( Repository(..)
  , initRepository
  , pickInRepository
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, singleton, (!!), head)
import Data.Maybe (Maybe(..))

type Repository a =
  { total :: Int
  , options :: NonEmptyArray { chance :: Int, value :: a }
  }

initRepository :: forall a. Int -> a -> Repository a
initRepository chance value =
  { total: chance
  , options: singleton { chance, value }
  }

pickInRepository :: forall a. Repository a -> Int -> a
pickInRepository { options } = find 0

  where

  find :: Int -> Int -> a
  find i n = case options !! i of
    Nothing -> (head options).value
    Just { chance, value } | chance > n -> value
    Just { chance } -> find (i + 1) (n - chance)
