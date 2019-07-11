module Ritoppu.Model.Game
  ( Game(..)
  , GameState(..)
  , gameIsOver
  ) where

import Ritoppu.Model.Stage (Stage)

data GameState
  = Idle
  | Dead

type Game =
  { stage :: Stage
  , state :: GameState
  }

gameIsOver :: Game -> Boolean
gameIsOver game = case game.state of
  Dead -> true
  _ -> false
