module Ritoppu.Component.App
  ( component
  , Query(..)
  ) where

import Prelude hiding (div)

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ritoppu.Random (runGenerator, randomSeed)
import Ritoppu.Action (ActionResult, inactive)
import Ritoppu.Action.Move (move)
import Ritoppu.Display (build)
import Ritoppu.Model (Direction(..), Game)
import Ritoppu.DungeonGenerator (generator)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

data Action
  = InitGame

type Message = Void
data Query a
  = KeyboardDown KeyboardEvent a

type State = { game :: Maybe Game }

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
        , initialize = Just InitGame
        }
    }

initialState :: State
initialState = { game: Nothing }

render :: forall p i. State -> HH.HTML p i
render app = case app.game of
  Just game -> div "app-container"
    [ div "level-map" $
        map
            (div "row")
            (build game.stage)
    ]
  Nothing -> div "" [ HH.text "Loading..." ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  InitGame -> do
    seed <- H.liftEffect $ randomSeed
    H.put { game: Just { stage: runGenerator seed (generator { x: 30, y: 30 }) } }
    pure unit

onGame :: (Game -> Game) -> State -> State
onGame f app = app { game = f <$> app.game }

handleQuery :: forall a. Query a -> H.HalogenM State Action () Message Aff (Maybe a)
handleQuery (KeyboardDown ev next) = do
  H.modify_ (onGame (\game -> ((keyToAction (KE.key ev)) game).result))
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
