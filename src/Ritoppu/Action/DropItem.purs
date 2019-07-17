module Ritoppu.Action.DropItem
  ( dropItem
  ) where

import Prelude

import Ritoppu.Action (Action(..), ActionResult, Message(..), addAction)
import Ritoppu.Action.CreatureAct (creatureAct)
import Ritoppu.Model (Game, Item)
import Ritoppu.Mutation (addItem, removeItemFromInventory)

dropItem :: Item -> Game -> ActionResult Game
dropItem item game
  = addAction (LogMessage (DropItem item))
  $ creatureAct (playerTurn (removeItem item withItem))

  where

  withItem :: Game
  withItem = game { stage = addItem game.stage.player.pos item game.stage }

-- TODO: Remove duplicity
playerTurn :: Game -> Game
playerTurn game = game { stage { player { turn = game.stage.player.turn + 5 } } }

-- TODO: Remove duplicity
removeItem :: Item -> Game -> Game
removeItem item game =
  game { stage { player { inventory = removeItemFromInventory item game.stage.player.inventory } } }
