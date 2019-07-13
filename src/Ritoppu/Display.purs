module Ritoppu.Display
  ( DisplayTile(..)
  , build
  ) where

import Prelude hiding (div)

import Data.Array (range)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ritoppu.Model (CreatureType(..), Item(..), Point, Stage, Tile(..), creatureAt, creatureName, isSeenTile, isVisibleTile, itemAt, playerAt, tileAt)

type DisplayTile = forall p i. HH.HTML p i

div :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
div classes = HH.div [ HP.class_ (wrap classes) ]

build :: forall p i. Stage -> Array (Array (HH.HTML p i))
build stage = map
  (\y -> map
      (\x -> buildElem stage { x, y })
      (range 0 stage.size.x))
  (range 0 stage.size.y)

buildElem :: Stage -> Point -> DisplayTile
buildElem stage pos = case { creature: creatureAt stage pos, item: itemAt stage pos } of -- EXTRA: Speed it up
  _ | not (isSeenTile stage.fovMask pos)
      -> div "tile -nothing" []
  _ | not (isVisibleTile stage.fovMask pos)
      -> displayTile " -seen" tile []
  { creature: Just creature }
      -> displayTile "" tile
        [ HH.div
          [ HP.class_ (wrap ("creature " <> creatureClass creature.type))
          , HP.title (creatureName creature)
          ]
          []
        ]
  _ | playerAt stage pos && stage.player.stats.hp <= 0 -- EXTRA: prettify method
      -> displayTile "" tile [ div "creature -player -corpse" [] ]
  _ | playerAt stage pos
      -> displayTile "" tile [ div "creature -player" [] ]
  { item: Just item }
      -> displayTile "" tile [ div (itemClass item) [] ]
  _ -> displayTile "" tile []

  where

  tile = tileAt stage pos

  displayTile :: forall p i. String -> Tile -> Array (HH.HTML p i) -> HH.HTML p i
  displayTile var = case _ of
    Floor -> div ("tile -floor" <> var)
    Wall -> div ("tile -wall" <> var)

creatureClass :: CreatureType -> String
creatureClass = case _ of
  RedNagaHatchling -> "-red_naga_hatchling"
  RedNaga -> "-red_naga"

-- EXTRA: Clean up variants and elements
itemClass :: Item -> String
itemClass = case _ of
  Corpse creature -> "creature -corpse " <> creatureClass creature
