module Ritoppu.Action.IncreaseStats
  ( increaseMaxHp
  , increasePower
  , increaseDefense
  ) where

import Prelude

import Ritoppu.Action (ActionResult)
import Ritoppu.Action.CreatureAct (creatureAct)
import Ritoppu.Model (Game)

-- TODO: Shouldn't be here. Rename to level up or something
increaseMaxHp :: Int -> Game -> ActionResult Game
increaseMaxHp dh game
  = creatureAct game { stage { player { stats { hp = hp + dh, maxHp = maxHp + dh } } } }

  where

  { hp, maxHp } = game.stage.player.stats

increasePower :: Int -> Game -> ActionResult Game
increasePower dp game
  = creatureAct game { stage { player { stats { power = game.stage.player.stats.power + dp } } } }

increaseDefense :: Int -> Game -> ActionResult Game
increaseDefense df game
  = creatureAct game { stage { player { stats { defense = game.stage.player.stats.defense + df } } } }
