module Ritoppu.Mutation.Repository
  ( addToRepository
  ) where

import Prelude

import Data.Array.NonEmpty (snoc)
import Ritoppu.Model (Repository)

addToRepository :: forall a. Int -> a -> Repository a -> Repository a
addToRepository chance value { total, options }
  = { total: total + chance, options: snoc options { chance, value } }
