module Ritoppu.Model.Player
  ( Player
  , initPlayer
  ) where

import Ritoppu.Model.Point (Point)
import Ritoppu.Model.Stats (Stats)

type Player = { pos :: Point, stats :: Stats, turn :: Int }

initPlayer :: Player
initPlayer =
  { pos: { x: 0, y: 0 }
  , stats: { maxHp: 30, hp: 30, defense: 2, power: 5 }
  , turn: 0
  }
