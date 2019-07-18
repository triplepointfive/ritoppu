module Ritoppu.Component.App
  ( component
  , Query(..)
  ) where

import Prelude hiding (div)

import Data.Array (concatMap, take, (:))
import Data.Foldable (foldM)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ritoppu.Action (ActionResult, inactive)
import Ritoppu.Action as A
import Ritoppu.Action.DropItem (dropItem)
import Ritoppu.Action.Move (move)
import Ritoppu.Action.PickUp (pickUp)
import Ritoppu.Action.UseItem (useItem)
import Ritoppu.Display (build)
import Ritoppu.DisplayLog (loggerBlock)
import Ritoppu.DungeonGenerator (generator)
import Ritoppu.Model (Direction(..), Game, Item, Point, inventoryPositions, itemName)
import Ritoppu.Mutation (updateFov)
import Ritoppu.Random (runGenerator, randomSeed)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

data Action
  = InitGame
  | MouseClick Point

type Message = Void
data Query a
  = KeyboardDown KeyboardEvent a

type State = { state :: GameState, logs :: Array A.Message }

data GameState
  = Idle Game
  | Dead Game
  | UseItem Game -- EXTRA: Move numered inventory here?
  | DropItem Game
  | Targeting Game (Point -> Game -> ActionResult Game)
  | Init

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
initialState = { state: Init, logs: [] }

render :: forall m. State -> HH.ComponentHTML Action () m
render app = div "app-container" case app.state of
  Idle game ->
    [ gameInterface game
    , loggerBlock app.logs
    , sidebar game
    ]
  Dead game ->
    [ gameInterface game
    , loggerBlock app.logs
    , div "screen -dead" [ HH.text "YOU DIED" ]
    , sidebar game
    ]
  Targeting game f ->
    [ div "level-map"
      $ map
          (div "row")
          (build (\p -> [ HE.onClick (\_ -> Just (MouseClick p)) ]) game.stage)
    , loggerBlock app.logs
    , sidebar' game
    ]
  UseItem game ->
    [ gameInterface game
    , loggerBlock app.logs
    , sidebar' game
    ]
  DropItem game ->
    [ gameInterface game
    , loggerBlock app.logs
    , sidebar' game
    ]
  Init ->
    [ HH.text "Loading..."
    ]

gameInterface :: forall p i. Game -> HH.HTML p i
gameInterface game = div "level-map" $ map (div "row") (build (const []) game.stage)

-- TODO: Shame on me
sidebar' :: forall p i. Game -> HH.HTML p i
sidebar' game =
  div "panel-sidebar"
    [ div "stats"
        [ HH.dl []
            [ HH.dd [] [ HH.text "HP" ]
            , HH.dt [] [ HH.text (show game.stage.player.stats.hp <> " / " <> show game.stage.player.stats.maxHp) ]
            ]
        ]
    , HH.text "Inventory:"
    , div "stats"
        [ HH.dl [] inventoryItems'
        ]
    ]

  where

  inventoryItems' =
    concatMap
      (\(Tuple letter (Tuple item count)) ->
        [ HH.dd [] [ HH.text (letter <> ": " <> itemName item) ]
        , HH.dt [] [ HH.text (show count) ]
        ]
      )
    $ Map.toUnfoldable $ inventoryPositions game.stage.player.inventory

sidebar :: forall p i. Game -> HH.HTML p i
sidebar game =
  div "panel-sidebar"
    [ div "stats"
        [ HH.dl []
            [ HH.dd [] [ HH.text "HP" ]
            , HH.dt [] [ HH.text (show game.stage.player.stats.hp <> " / " <> show game.stage.player.stats.maxHp) ]
            ]
        ]
    , HH.text "Inventory:"
    , div "stats"
        [ HH.dl [] (inventoryItems game)
        ]
    ]

inventoryItems :: forall p i. Game -> Array (HH.HTML p i)
inventoryItems game =
  concatMap
    (\(Tuple item count) ->
      [ HH.dd [] [ HH.text (itemName item) ]
      , HH.dt [] [ HH.text (show count) ]
      ]
    )
  $ Map.toUnfoldable game.stage.player.inventory

handleAction :: Action -> H.HalogenM State Action () Message Aff Unit
handleAction = case _ of
  InitGame -> do
    seed <- H.liftEffect randomSeed
    -- H.modify_ (_
    --   { state = Targeting
    --     { stage: updateFov $ runGenerator seed (generator { x: 30, y: 30 }) }
    --     castFireball
    --   })
    H.modify_ (_ { state = Idle
        { stage: updateFov $ runGenerator seed (generator { x: 30, y: 30 })
        } })
    pure unit
  MouseClick point -> do
    { state } <- H.get
    case state of
      Targeting game f -> do
        action (f point) game
      _ ->
        pure unit

handleQuery :: forall a. Query a -> H.HalogenM State Action () Message Aff (Maybe a)
handleQuery = case _ of
  KeyboardDown ev next -> do
    state <- H.get
    case state.state of
      Idle game -> do
        idleKeyAct (KE.key ev) game
        pure (Just next)
      UseItem game -> do
        withItemKeyAct (KE.key ev) useItem game
        pure (Just next)
      DropItem game -> do
        withItemKeyAct (KE.key ev) dropItem game
        pure (Just next)
      Targeting game _ -> do
        cancelKeyAct (KE.key ev) game
        pure (Just next)
      Dead _ ->
        pure (Just next)
      Init ->
        pure (Just next)

processAction :: forall m. Bind m => MonadAff m => State -> A.Action -> m State
processAction state = case _ of
  A.LogMessage message -> do
      pure state { logs = take 5 (message : state.logs) }
  A.Die game -> do
      pure state { state = Dead game }
  A.Target game f -> do
      pure state { state = Targeting game f }

action :: (Game -> ActionResult Game) -> Game -> H.HalogenM State Action () Message Aff Unit
action f game = do
  state <- H.get
  foldM processAction (state { state = Idle result }) actions >>= H.put

  where

  { result, actions } = f game

idleKeyAct :: String -> Game -> H.HalogenM State Action () Message Aff Unit
idleKeyAct = case _ of
  "y" -> action $ move NW
  "u" -> action $ move NE
  "b" -> action $ move SW
  "n" -> action $ move SE
  "h" -> action $ move W
  "j" -> action $ move S
  "k" -> action $ move N
  "l" -> action $ move E
  "ArrowLeft" -> action $ move W
  "ArrowDown" -> action $ move S
  "ArrowUp" -> action $ move N
  "ArrowRight" -> action $ move E
  "," -> action pickUp
  "g" -> action pickUp
  "a" -> \game -> H.modify_ (_ { state = UseItem game })
  "d" -> \game -> H.modify_ (_ { state = DropItem game })
  _ -> action inactive

cancelKeyAct :: String -> Game -> H.HalogenM State Action () Message Aff Unit
cancelKeyAct key game = case key of
  "Escape" -> H.modify_ (_ { state = Idle game })
  _ -> pure unit

withItemKeyAct :: String -> (Item -> Game -> ActionResult Game) -> Game -> H.HalogenM State Action () Message Aff Unit
withItemKeyAct key f game = case { key: key, item: foundItem } of
  { key: "Escape" } -> H.modify_ (_ { state = Idle game })
  { item: Nothing } -> do
      state <- H.get
      processAction state (A.LogMessage A.DoNotHave) >>= H.put
      H.modify_ (_ { state = Idle game })
  { item: Just (Tuple item _) } -> do
      action (f item) game

  where

  foundItem = Map.lookup key (inventoryPositions game.stage.player.inventory)
