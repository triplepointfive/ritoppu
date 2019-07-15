module Ritoppu.Action
  ( Action(..)
  , ActionResult
  , Message(..)
  , addAction
  , inactive
  , withAction
  , onResult
  ) where

import Data.Array ((:))
import Ritoppu.Model (Creature, Item)

data Message
  = DamageYouM Creature Int
  | DamageYouHarmlessM Creature
  | HitAWallM
  | AttackM Creature Int
  | AttackHarmlessM Creature
  | AttackKillM Creature
  | NothingToPickUp
  | PickedUpItem Item

data Action
  = LogMessage Message

type ActionResult a =
  { result :: a
  , actions :: Array Action
  }

addAction :: forall a. Action -> ActionResult a -> ActionResult a
addAction action { result, actions } = { result, actions: action : actions }

inactive :: forall a. a -> ActionResult a
inactive result = { result, actions: [] }

withAction :: forall a. a -> Action -> ActionResult a
withAction result action = { result, actions: [action] }

onResult :: forall a. (a -> a) -> ActionResult a -> ActionResult a
onResult f { result, actions } = { result: f result, actions }
