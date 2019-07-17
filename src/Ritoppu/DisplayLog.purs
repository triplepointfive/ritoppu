module Ritoppu.DisplayLog
  ( loggerBlock
  ) where

import Prelude hiding (div)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ritoppu.Action (Message(..))
import Ritoppu.Model (creatureName, itemName)

div :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
div classes = HH.div [ HP.class_ (H.ClassName classes) ]

loggerBlock :: forall p i. Array Message -> HH.HTML p i
loggerBlock logs =
  div "logger-block" (map buildMessage logs)

buildMessage :: forall p i. Message -> HH.HTML p i
buildMessage msg = div "log-message" $ case msg of
  DamageYouM creature damage
    -> [ warn (creatureName creature)
       , info " attacks you for "
       , critical (show damage)
       , info " hit points"
       ]
  DamageYouHarmlessM creature
    -> [ warn (creatureName creature), info " attacks you but does no damage" ]
  HitAWallM
    -> [ debug "You hit a wall " ]
  AttackM creature damage
    -> [ info "You attacked "
      , warn (creatureName creature)
      , info " and did "
      , warn (show damage)
      , info " damage"
      ]
  AttackHarmlessM creature
    -> [ info "You attacked "
      , critical (creatureName creature)
      , info " but did no damage"
      ]
  AttackKillM creature
    -> [ info "You smashed ", critical (creatureName creature) ]
  PickedUpItem item
    -> [ info "You picked ", focus (itemName item), info " up" ]
  NothingToPickUp
    -> [ debug "You see nothing to pick up" ]
  DropItem item
    -> [ info "You dropped ", focus (itemName item) ]
  DoNotHave
    -> [ debug "You don't have this" ]

debug :: forall p i. String -> HH.HTML p i
debug = withStyle "msg -debug"

critical :: forall p i. String -> HH.HTML p i
critical = withStyle "msg -critical"

warn :: forall p i. String -> HH.HTML p i
warn = withStyle "msg -warn"

info :: forall p i. String -> HH.HTML p i
info = withStyle "msg -info"

focus :: forall p i. String -> HH.HTML p i
focus = withStyle "msg -focus"

withStyle :: forall p i. String -> String -> HH.HTML p i
withStyle classes msg = HH.span [ HP.class_ (H.ClassName classes) ] [ HH.text msg ]
