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
import Ritoppu.Action (ActionResult, inactive)
import Ritoppu.Action.Move (move)
import Ritoppu.Display (build)
import Ritoppu.Model (Direction(..), Game, Stage)
import Ritoppu.Model.Tile (Tile(..)) as T
import Ritoppu.Generator (generate)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

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
initialState = { stage: generate 15 stage }

  where

  stage :: Stage
  stage =
    { player: { pos: { x: 15, y: 15 } }
    , tiles: Map.empty
    , size: { x: 32, y: 32 }
    }

render :: forall p i. State -> HH.HTML p i
render game = div "app-container"
  [ div "level-map" $
      map
          (div "row")
          (build game.stage)
  ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction _ = do
  pure unit

handleQuery :: forall a. Query a -> H.HalogenM State Action () Message Aff (Maybe a)
handleQuery (KeyboardDown ev next) = do
  H.modify_ (\game -> ((keyToAction (KE.key ev)) game).result)
  pure (Just next)

keyToAction :: String -> (Game -> ActionResult Game)
keyToAction = case _ of
  "y" -> move NW
  "u" -> move NE
  "b" -> move SW
  "n" -> move SE
  "h" -> move W
  "j" -> move S
  "k" -> move N
  "l" -> move E
  "ArrowLeft" -> move W
  "ArrowDown" -> move S
  "ArrowUp" -> move N
  "ArrowRight" -> move E
  _ -> inactive
