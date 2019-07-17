module Ritoppu.Action.UseItem
  ( useItem
  ) where

import Ritoppu.Action (ActionResult, inactive)
import Ritoppu.Model (Game, Item)

useItem :: Item -> Game -> ActionResult Game
useItem item = inactive
