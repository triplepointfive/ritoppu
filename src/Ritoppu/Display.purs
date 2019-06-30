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
import Ritoppu.Model (Point, Stage, Tile(..), Creature, CreatureType(..), creatureAt, isSeenTile, isVisibleTile, playerAt, tileAt)

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
buildElem stage pos = case creatureAt stage pos of
  _ | not (isSeenTile stage.fovMask pos)
      -> div "tile -nothing" []
  _ | not (isVisibleTile stage.fovMask pos)
      -> displayTile " -seen" tile []
  Just creature
      -> displayTile "" tile [ div ("creature " <> creatureClass creature) [] ]
  _ | playerAt stage pos
      -> displayTile "" tile [ div "creature -player" [] ]
  _ -> displayTile "" tile []

  where

  tile = tileAt stage pos

  displayTile :: forall p i. String -> Tile -> Array (HH.HTML p i) -> HH.HTML p i
  displayTile var = case _ of
    Floor -> div ("tile -floor" <> var)
    Wall -> div ("tile -wall" <> var)

creatureClass :: Creature -> String
creatureClass creature = case creature.type of
  RedNagaHatchling -> "-red_naga_hatchling"
  RedNaga -> "-red_naga"
