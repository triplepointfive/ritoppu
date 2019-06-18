module Ritoppu.Component.App
  ( component
  , Query(..)
  ) where

import Prelude hiding (div)

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ritoppu.Action.Move (move)
import Ritoppu.Display (build, displayTileToText)
import Ritoppu.Model (Direction(..), Game, Stage)
import Ritoppu.Model.Tile (Tile(..)) as T
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

data Action = SetRadius Int

type Message = Void
data Query a
  = KeyboardDown KeyboardEvent a

type State = Game

div :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
div classes = HH.div [ HP.class_ (H.ClassName classes) ]

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
initialState = { stage: stage }

  where

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

render :: forall p i. State -> HH.HTML p i
render game = div "app-container"
  [ div "level-map" $
      map
          (div "row" <<< map (HH.text <<< displayTileToText))
          (build game.stage)
  ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction _ = do
  pure unit

handleQuery :: forall a. Query a -> H.HalogenM State Action () Message Aff (Maybe a)
handleQuery (KeyboardDown ev next) = do
  H.modify_ (\game -> (move E game).result)
  pure (Just next)
