module Ritoppu.Action
  ( Action(..)
  , ActionResult
  , addAction
  , inactive
  , withAction
  , onResult
  ) where

import Data.Array ((:))

data Action
  = LogMessage String

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
