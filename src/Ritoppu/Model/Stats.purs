module Ritoppu.Model.Stats
  ( Stats
  ) where

import Prelude

type Stats =
  { maxHp :: Int
  , hp :: Int
  , defense :: Int
  , power :: Int
  }
