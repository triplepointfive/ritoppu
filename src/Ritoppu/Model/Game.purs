module Ritoppu.Model.Game
  ( Game(..)
  ) where

import Prelude

import Ritoppu.Model.Stage (Stage)

type Game =
  { stage :: Stage
  }
