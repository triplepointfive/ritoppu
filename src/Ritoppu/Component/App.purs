module Ritoppu.Component.App
  ( component
  , Query(..)
  ) where

import Prelude hiding (div)

import Data.Array ((:), take)
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ritoppu.Action (ActionResult, inactive)
import Ritoppu.Action as A
import Ritoppu.Action.Move (move)
import Ritoppu.Display (build)
import Ritoppu.DungeonGenerator (generator)
import Ritoppu.Model (Direction(..), Game, GameState(..), gameIsOver)
import Ritoppu.Mutation (updateFov)
import Ritoppu.Random (runGenerator, randomSeed)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

data Action
  = InitGame

type Message = Void
data Query a
  = KeyboardDown KeyboardEvent a

type State = { game :: Maybe Game, logs :: Array String }

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
initialState = { game: Nothing, logs: [] }

render :: forall p i. State -> HH.HTML p i
render app = case app.game of
  Just game -> div "app-container"
    [ div "level-map" $
        map
            (div "row")
            (build game.stage)
    , div "logger-block" (map (\log -> div "message" [ HH.text log ]) app.logs)
    , stateInterface game
    , sidebar game
    ]
  Nothing -> div "" [ HH.text "Loading..." ]

stateInterface :: forall p i. Game -> HH.HTML p i
stateInterface game = case game.state of
  Idle -> div "" []
  Dead -> div "screen -dead" [ HH.text "YOU DIED" ]

sidebar :: forall p i. Game -> HH.HTML p i
sidebar game =
  div "panel-sidebar"
    [ div "stats"
        [ HH.dl []
            [ HH.dd [] [ HH.text "HP" ]
            , HH.dt [] [ HH.text (show game.stage.player.stats.hp <> " / " <> show game.stage.player.stats.maxHp) ]
            ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  InitGame -> do
    seed <- H.liftEffect randomSeed
    H.modify_ (_ { game = Just
        { stage: updateFov $ runGenerator seed (generator { x: 30, y: 30 })
        , state: Idle
        } })
    pure unit

onGame :: (Game -> Game) -> State -> State
onGame f app = app { game = f <$> app.game }

handleQuery :: forall a. Query a -> H.HalogenM State Action () Message Aff (Maybe a)
handleQuery (KeyboardDown ev next) = do
  state <- H.get
  case state.game of
    Just game | not (gameIsOver game) -> do
      let { result, actions } = (keyToAction (KE.key ev)) game
      foldM processAction (state { game = Just result }) actions >>= H.put
      pure (Just next)
    _ ->
      pure (Just next)

processAction :: forall m. Bind m => MonadAff m => State -> A.Action -> m State
processAction state = case _ of
  A.LogMessage sound -> do
      pure state { logs = take 5 (sound : state.logs) }

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
