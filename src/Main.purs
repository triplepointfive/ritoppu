module Main where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (EventType)
import Web.Event.EventTarget as ET
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import Web.HTML.HTMLDocument (HTMLDocument, toEventTarget)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Ritoppu.Component.App (component, Query(..))

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  game <- runUI component unit body

  document <- H.liftEffect $ DOM.document =<< DOM.window
  liftEffect $ do
      on KET.keydown document (game.query <<< H.tell <<< KeyboardDown)

  pure unit

on :: forall a. EventType -> HTMLDocument -> (KeyboardEvent -> Aff a) -> Effect Unit
on eventType document fn = do
  listener <- ET.eventListener (traverse_ (void <<< launchAff <<< fn) <<< KE.fromEvent)
  ET.addEventListener eventType listener false (toEventTarget document)
