module Ritoppu.Model.Inventory
  ( Inventory(..)
  , initInventory
  ) where

import Data.Map as Map
import Ritoppu.Model.Item (Item)

type Inventory = Map.Map Item Int

initInventory :: Inventory
initInventory = Map.empty
