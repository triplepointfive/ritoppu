module Ritoppu.Action.UseItem
  ( useItem
  ) where

import Prelude

import Ritoppu.Action (Action(..), ActionResult, Message(..), addAction, withAction)
import Ritoppu.Action.CreatureAct (creatureAct)
import Ritoppu.Model (Game, Item(..), isFullHealth)
import Ritoppu.Mutation (heal, removeItemFromInventory)

useItem :: Item -> Game -> ActionResult Game
useItem = case _ of
  HealingPotion -> useHealingPotion
  Corpse _ -> flip withAction (LogMessage DoNotKnowHowToUse)

useHealingPotion :: Game -> ActionResult Game
useHealingPotion game = case unit of
  _ | isFullHealth game.stage.player.stats ->
    withAction game (LogMessage FullHealth)
  _
    -> addAction (LogMessage Healed)
    -- TODO: Magic number
    $ creatureAct (removeItem HealingPotion $ playerTurn game { stage { player { stats = heal 4 game.stage.player.stats } } } )

-- TODO: Remove duplicity
playerTurn :: Game -> Game
playerTurn game = game { stage { player { turn = game.stage.player.turn + 5 } } }

-- TODO: Remove duplicity
removeItem :: Item -> Game -> Game
removeItem item game =
  game { stage { player { inventory = removeItemFromInventory item game.stage.player.inventory } } }
