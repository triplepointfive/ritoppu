module Ritoppu.Component.App
  ( component
  , Action(..)
  , Query(..)
  ) where

import Prelude hiding (div)

import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

import Ritoppu.Display (DisplayTile(..), build)
import Ritoppu.Model (Stage)
import Ritoppu.Model.Tile (Tile(..)) as T

data Action = SetRadius Int

type Message = Void
data Query a
  = KeyboardDown KeyboardEvent a
type State = Game

type Game = Unit

div :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
div classes = HH.div [ HP.class_ (H.ClassName classes) ]

dl :: forall a p i. Show a => String -> Maybe a -> HH.HTML p i
dl term description =
  div "data-list"
    [ div "term" [ HH.text term ]
    , div "description"
        [ HH.text "888"
        , div (if isNothing description then "value -low" else "value")
              [ HH.text (maybe "---" show description) ]
        ]
    ]

-- | Top game component
component :: H.Component HH.HTML Query Unit Message Aff
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleQuery = handleQuery
        , handleAction = handleAction
        }
    }

initialState :: State
initialState = unit

stage :: Stage
stage =
  { player: { pos: { x: 1, y: 1 } }
  , tiles: Map.fromFoldable
      [ Tuple { x: 0, y: 0 } T.Wall
      , Tuple { x: 0, y: 1 } T.Wall
      , Tuple { x: 0, y: 2 } T.Wall
      , Tuple { x: 0, y: 3 } T.Wall

      , Tuple { x: 1, y: 0 } T.Wall
      , Tuple { x: 1, y: 1 } T.Floor
      , Tuple { x: 1, y: 2 } T.Floor
      , Tuple { x: 1, y: 3 } T.Wall

      , Tuple { x: 2, y: 0 } T.Wall
      , Tuple { x: 2, y: 1 } T.Floor
      , Tuple { x: 2, y: 2 } T.Floor
      , Tuple { x: 2, y: 3 } T.Wall

      , Tuple { x: 3, y: 0 } T.Wall
      , Tuple { x: 3, y: 1 } T.Wall
      , Tuple { x: 3, y: 2 } T.Wall
      , Tuple { x: 3, y: 3 } T.Wall
      ]
  , size: { x: 4, y: 4 }
  }

render :: forall t38 t39 t40. t38 -> HH.HTML t40 t39
render game =
  div "app-container"
    [ renderDisplayMap stage ]

renderDisplayMap levelMap =
  div "level-map" $
    map (div "row" <<< map (HH.text <<< displayTileToText)) (build levelMap)

displayTileToText = case _ of
  Wall -> "#"
  Floor -> "."
  Creature ch _ -> singleton ch
  Empty -> " "

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction _ = do
  pure unit

handleQuery :: forall a. Query a -> H.HalogenM State Action () Message Aff (Maybe a)
handleQuery (KeyboardDown ev next) = do
  pure (Just next)
