module Ritoppu.Model.Player
  ( Player
  , initPlayer
  ) where

import Ritoppu.Model.Inventory (Inventory, initInventory)
import Ritoppu.Model.Level (Level, initLevel)
import Ritoppu.Model.Point (Point)
import Ritoppu.Model.Stats (Stats)
import Ritoppu.Model.Equipment (Equipment, initEquipment)

type Player =
  { pos :: Point
  , stats :: Stats
  , turn :: Int
  , inventory :: Inventory
  , level :: Level
  , equipment :: Equipment
  }

initPlayer :: Player
initPlayer =
  { pos: { x: 0, y: 0 }
  , stats: { maxHp: 100, hp: 100, defense: 1, power: 4 }
  , turn: 0
  , inventory: initInventory
  , level: initLevel
  , equipment: initEquipment
  }
