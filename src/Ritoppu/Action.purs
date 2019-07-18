module Ritoppu.Action
  ( Action(..)
  , ActionResult
  , Message(..)
  , addAction
  , addActions
  , die
  , inactive
  , onResult
  , withAction
  , target
  ) where

import Prelude

import Data.Array ((:))
import Ritoppu.Model (Creature, Game, Item, Point)

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
  | DoNotKnowHowToUse
  | Healed
  | FullHealth
  | LightningScrollHitYourself
  | TargetingOutOfFov
  | FireballExplodes Int
  | BurnSelf Int
  | BurnM Creature Int
  | Targeting String
  | TargetNoEnemy
  | CastConfusion Creature

data Action
  = LogMessage Message
  | Die Game
  | Target Game (Point -> Game -> ActionResult Game)

type ActionResult a =
  { result :: a
  , actions :: Array Action
  }

die :: ActionResult Game -> ActionResult Game
die { result, actions } = { result, actions: Die result : actions }

target :: (Point -> Game -> ActionResult Game) -> String -> ActionResult Game -> ActionResult Game
target f msg { result, actions } = { result, actions: Target result f : LogMessage (Targeting msg) : actions }

addAction :: forall a. Action -> ActionResult a -> ActionResult a
addAction action { result, actions } = { result, actions: action : actions }

addActions :: forall a. Array Action -> ActionResult a -> ActionResult a
addActions moreActions { result, actions } = { result, actions: moreActions <> actions }

inactive :: forall a. a -> ActionResult a
inactive result = { result, actions: [] }

withAction :: forall a. a -> Action -> ActionResult a
withAction result action = { result, actions: [action] }

onResult :: forall a. (a -> a) -> ActionResult a -> ActionResult a
onResult f { result, actions } = { result: f result, actions }
