module Ritoppu.Mutation.Inventory
  ( addItemToInventory
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Ritoppu.Model (Item, Inventory)

addItemToInventory :: Item -> Inventory -> Inventory
addItemToInventory item = Map.alter (Just <<< maybe 1 ((+) 1)) item
