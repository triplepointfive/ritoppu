module Ritoppu.Action.EnemyAct
  ( creatureAct
  ) where

import Prelude

import Data.Foldable (foldr)
import Ritoppu.Action (Action(..), ActionResult, addAction, inactive)
import Ritoppu.Model (Game, creatureName)

creatureAct :: Game -> ActionResult Game
creatureAct game@{ stage } =
  foldr
      (\creature -> addAction (LogMessage ("The " <> creatureName creature <> " ponders the meaning of its existence.")))
      (inactive game)
      (stage.creatures)
