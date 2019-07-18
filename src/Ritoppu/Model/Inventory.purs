module Ritoppu.Model.Inventory
  ( Inventory(..)
  , initInventory
  , inventoryPositions
  ) where

import Prelude

import Data.Array (zip, range)
import Data.Char (fromCharCode, toCharCode)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple(..))
import Ritoppu.Model.Item (Item(..))

type Inventory = Map.Map Item Int

initInventory :: Inventory
initInventory = Map.fromFoldable
  [ Tuple HealingPotion 3
  , Tuple LightningScroll 5
  , Tuple FireballScroll 2
  , Tuple ConfusionScroll 3
  ]

-- EXTRA: Limit per page
inventoryPositions :: Inventory -> Map.Map String (Tuple Item Int)
inventoryPositions
  = Map.fromFoldable
  <<< zip alphabet
  <<< Map.toUnfoldable

alphabet :: Array String
alphabet =
  map
    (singleton <<< fromMaybe 'a' <<< fromCharCode)
    (range (toCharCode 'a') (toCharCode 'z'))
