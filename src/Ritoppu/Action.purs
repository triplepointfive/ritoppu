module Ritoppu.Action
  ( Action(..)
  , ActionResult
  , Message(..)
  , addAction
  , inactive
  , withAction
  , onResult
  , die
  ) where

import Data.Array ((:))
import Ritoppu.Model (Creature, Item, Game)

data Message
  = DamageYouM Creature Int
  | DamageYouHarmlessM Creature
  | HitAWallM
  | AttackM Creature Int
  | AttackHarmlessM Creature
  | AttackKillM Creature
  | NothingToPickUp
  | PickedUpItem Item
  | DropItem Item
  | DoNotHave

data Action
  = LogMessage Message
  | Die Game

type ActionResult a =
  { result :: a
  , actions :: Array Action
  }

die :: ActionResult Game -> ActionResult Game
die { result, actions } = { result, actions: Die result : actions }

addAction :: forall a. Action -> ActionResult a -> ActionResult a
addAction action { result, actions } = { result, actions: action : actions }

inactive :: forall a. a -> ActionResult a
inactive result = { result, actions: [] }

withAction :: forall a. a -> Action -> ActionResult a
withAction result action = { result, actions: [action] }

onResult :: forall a. (a -> a) -> ActionResult a -> ActionResult a
onResult f { result, actions } = { result: f result, actions }
