module Ritoppu.Action.PickUp
  ( pickUp
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Ritoppu.Action (Action(..), ActionResult, Message(..), addAction, withAction)
import Ritoppu.Action.CreatureAct (creatureAct)
import Ritoppu.Model (Game, Item, itemAt)
import Ritoppu.Mutation (addItemToInventory, onStage, removeItem)

pickUp :: Game -> ActionResult Game
pickUp game@{ stage } = case itemAt stage pos of
  Just item
    -> addAction (LogMessage (PickedUpItem item))
    $ creatureAct (addItem item $ onStage (removeItem pos item) $ playerTurn game)
  _ -> withAction game (LogMessage NothingToPickUp)

  where

  pos = stage.player.pos


-- TODO: Remove duplicity
playerTurn :: Game -> Game
playerTurn game = game { stage { player { turn = game.stage.player.turn + 5 } } }

-- TODO: Remove duplicity
addItem :: Item -> Game -> Game
addItem item game =
  game { stage { player { inventory = addItemToInventory item game.stage.player.inventory } } }
