module Ritoppu.Component.App
  ( component
  , Query(..)
  ) where

import Prelude hiding (div)

import Data.Argonaut.Core (fromString, stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (decodeJArray)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (concatMap, take, (:))
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
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
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

saveGameStorageKey :: String
saveGameStorageKey = "savedGame"

autoSave :: Boolean
autoSave = true

data Action
  = InitGame
  | MouseClick Point

type Message = Void
data Query a
  = KeyboardDown KeyboardEvent a

type State = { state :: AppScreen, logs :: Array A.Message }

data AppScreen
  = Idle Game
  | Dead Game
  | UseItem Game -- EXTRA: Move numered inventory here?
  | DropItem Game
  | Targeting Game (Point -> Game -> ActionResult Game)
  | MainMenu (Maybe Game)
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
  MainMenu mGame -> [ div "main-menu"
    [ div "logo" [ HH.text "RITOPPU" ]
    -- TODO: Add ability to click on option
    , div "option" [ HH.text "[N]ew game" ]
    , div
      (if isJust mGame then "option" else "option -inactive")
      [ HH.text "[C]ontinue last game" ]
    , div "copyright" [ HH.text "By Ilya Smelkov" ]
    ]
  ]
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
    , HH.hr_
    , div "help-section"
        [ div "" [ HH.text "Arrows or vim-keys to move" ]
        , div "" [ HH.text "g or , to pick item up" ]
        , div "" [ HH.text "d to drop" ]
        , div "" [ HH.text "a to use an item" ]
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

    , HH.hr_
    , div "help-section"
        [ div "" [ HH.text "Arrows or vim-keys to move" ]
        , div "" [ HH.text "g or , to pick item up" ]
        , div "" [ HH.text "d to drop" ]
        , div "" [ HH.text "a to use an item" ]
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
    -- H.modify_ (_ { state = Idle
    --     { stage: updateFov $ runGenerator seed (generator { x: 30, y: 30 })
    --     } })
    s <- H.liftEffect (window >>= localStorage)
    mGame <- H.liftEffect loadGame
    H.modify_ (_ { state = MainMenu mGame })
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
      MainMenu mGame -> do
        mainMenuKeyAct mGame (KE.key ev)
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
      H.liftEffect deleteGame
      pure state { state = Dead game }
  A.Target game f -> do
      pure state { state = Targeting game f }

action :: (Game -> ActionResult Game) -> Game -> H.HalogenM State Action () Message Aff Unit
action f game = do
  app <- H.get
  foldM processAction (app { state = Idle result }) actions >>= H.put

  when autoSave do
    { state } <- H.get
    case state of
      Idle g ->
        H.liftEffect (saveGame g)
      _ -> pure unit

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

mainMenuKeyAct :: (Maybe Game) -> String -> H.HalogenM State Action () Message Aff Unit
mainMenuKeyAct mGame key = case { key, mGame } of
  { key: "n" } -> do
    seed <- H.liftEffect randomSeed
    let game = { stage: updateFov $ runGenerator seed (generator { x: 30, y: 30 }) }
    H.liftEffect (saveGame game)
    H.modify_ (_ { state = Idle game })
  { key: "c", mGame: Just game } -> do
    H.modify_ (_ { state = Idle game })
  _ -> pure unit

deleteGame :: Effect Unit
deleteGame = window >>= localStorage >>= removeItem saveGameStorageKey

saveGame :: Game -> Effect Unit
saveGame game = window >>= localStorage >>= setItem saveGameStorageKey (stringify $ encodeJson game)

loadGame :: Effect (Maybe Game)
loadGame = do
  mJsonGame <- window >>= localStorage >>= getItem saveGameStorageKey
  case mJsonGame of
    Just jsonGame -> do
      case jsonParser jsonGame of
        Left msg -> do
          log msg
          pure Nothing
        Right json -> do
          case decodeJson json of
            Left msg -> do
              log msg
              pure Nothing
            Right game -> pure game
    Nothing -> pure Nothing
