module Ritoppu.Model.Game
  ( Game(..)
  ) where

import Ritoppu.Model.Stage (Stage)

type Game =
  { stage :: Stage
  }
