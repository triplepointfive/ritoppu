module Ritoppu.Display
  ( DisplayTile(..)
  , build
  ) where

import Prelude hiding (div)

import Data.Array (range)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ritoppu.Fov (Mask)
import Ritoppu.Model (Stage, Point, Tile(..), playerAt, tileAt)

type DisplayTile = forall p i. HH.HTML p i

div :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
div classes = HH.div [ HP.class_ (wrap classes) ]

build :: forall p i. Mask -> Stage -> Array (Array (HH.HTML p i))
build mask stage = map
    (\y -> map
        (\x -> toDisplayTile stage mask { x, y })
        (range 0 stage.size.x))
    (range 0 stage.size.y)

toDisplayTile :: Stage -> Mask -> Point -> DisplayTile
toDisplayTile stage mask pos = case tileAt stage pos of
  _ | not (fromMaybe false (Map.lookup pos mask)) -> div "tile -nothing" []
  Just tile | playerAt stage pos
      -> stageTileToDisplayTile tile [ div "creature -player" [] ]
  Just tile -> stageTileToDisplayTile tile []
  Nothing -> stageTileToDisplayTile Wall []

stageTileToDisplayTile :: forall p i. Tile -> Array (HH.HTML p i) -> HH.HTML p i
stageTileToDisplayTile = case _ of
  Floor -> div "tile -floor"
  Wall -> div "tile -wall"
