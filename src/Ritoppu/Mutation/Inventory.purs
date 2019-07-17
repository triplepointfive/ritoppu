module Ritoppu.Mutation.Inventory
  ( addItemToInventory
  , removeItemFromInventory
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Ritoppu.Model (Item, Inventory)

addItemToInventory :: Item -> Inventory -> Inventory
addItemToInventory item = Map.alter (Just <<< maybe 1 ((+) 1)) item

removeItemFromInventory :: Item -> Inventory -> Inventory
removeItemFromInventory item = Map.alter takeItem item
  where

  takeItem = case _ of
    Just 1 -> Nothing
    Nothing -> Nothing
    Just x -> Just (x - 1)
