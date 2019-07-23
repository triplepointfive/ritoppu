module Ritoppu.Action.CreatureAct
  ( creatureAct
  , wavePath
  ) where

import Prelude

import Data.Array (concatMap, filter, find, head, last, nub, null, (:))
import Data.Foldable (any, foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Ritoppu.Action (Action(..), ActionResult, Message(..), addAction, die, inactive, onResult, openLevelUp)
import Ritoppu.Model (AiStrategy(..), Creature, Game, Point, Stage, adjustPoints, anybodyAt, availableToMoveTo, damageTo, isNextTo, isReadyToLevelUp)
import Ritoppu.Mutation (moveCreature, updateCreature, takeDamage)
import Ritoppu.Mutation.Level (levelIncrease)

-- TODO: Creatures must not to walk on each other
-- TODO: Move leveling out of here
-- TODO: change isReadyToLevelUp?
creatureAct :: Game -> ActionResult Game
creatureAct game = if isReadyToLevelUp game.stage.player.level 0
  then openLevelUp $ inactive game { stage { player { level = levelIncrease 0 game.stage.player.level } } }
  else actNext $ inactive game

actNext :: ActionResult Game -> ActionResult Game
actNext result@{ result: game } = case head creaturesToAct of
  Just creature -> actCreature creature result
  Nothing -> result

  where

  -- EXTRA: Act only when visible
  creaturesToAct :: Array (Tuple Point Creature)
  creaturesToAct = filter
    (\(Tuple _ { turn }) -> game.stage.player.turn > turn)
    (Map.toUnfoldableUnordered game.stage.creatures)

actCreature :: Tuple Point Creature -> ActionResult Game -> ActionResult Game
actCreature cr@(Tuple pos creature) result = case creature.aiStrategy of
  ConfusedAI 0 prevAI
    ->
      actNext $
      onResult
        (\g -> g { stage = updateCreature pos (\c -> c { aiStrategy = prevAI }) g.stage })
        result
  ConfusedAI step prevAI
    -> case unit of
      _ | result.result.stage.player.pos == randomDest
        -> attack cr
        $ onResult
            (\g -> g { stage = updateCreature pos (\c -> c { aiStrategy = ConfusedAI (step - 1) prevAI }) g.stage })
            result

      _ | anybodyAt result.result.stage randomDest -- TODO: Attack in this case
        -> actNext
          $ onResult
              (\g -> g { stage = updateCreature pos (\c -> acted $ c { aiStrategy = ConfusedAI (step - 1) prevAI }) g.stage })
              result

      _ | availableToMoveTo result.result.stage randomDest -> actNext
        $ onResult (\game -> game
          { stage = moveCreature pos randomDest (acted creature { aiStrategy = ConfusedAI (step - 1) prevAI }) game.stage }
          )
            result

      _ -> actNext
          $ onResult
              (\g -> g { stage = updateCreature pos (\c -> acted $ c { aiStrategy = ConfusedAI (step - 1) prevAI }) g.stage })
              result

  _ | isNextTo pos result.result.stage.player.pos
    -> attack cr result
  _ -> actNext $
    onResult (\game -> game
      { stage = moveCreature pos (moveForward pos game.stage) (acted creature) game.stage }
      ) result

  where

  randomDest = pos - { x: dx , y: dy }

  dx = case unit of
    _ | mod pos.y 3 == 0 -> 1
    _ | mod pos.y 5 == 0 -> -1
    _ -> 0

  dy = case unit of
    _ | mod pos.x 3 == 0 -> 1
    _ | mod pos.x 5 == 0 -> -1
    _ -> 0

attack :: Tuple Point Creature -> ActionResult Game -> ActionResult Game
attack (Tuple pos creature) result = case damage of
  0 -> actNext $
    addAction
      (LogMessage (DamageYouHarmlessM creature))
      withActedCreature
  _ | damage >= result.result.stage.player.stats.hp ->
    addAction (LogMessage (DamageYouM creature damage))
    $ die
    $ onResult (\game -> game
        { stage { player { stats = takeDamage damage game.stage.player.stats } }
        } )
    withActedCreature
  _ -> actNext $
    addAction
      (LogMessage (DamageYouM creature damage))
      $ onResult (\game -> game { stage { player { stats = takeDamage damage game.stage.player.stats } } } )
      withActedCreature

  where

  damage = damageTo creature.stats result.result.stage.player.stats

  withActedCreature =
    onResult (\game -> game
      { stage = updateCreature pos acted game.stage
      }) result

acted :: Creature -> Creature
acted c = c { turn = c.turn + 5 }

moveForward :: Point -> Stage -> Point
moveForward origin stage@{ player: { pos } } =
  case wavePath origin pos cellAvailable of
      Just path -> case last path of
        Just dest | not (anybodyAt stage dest) -> dest
        _ -> origin
      _ -> origin

  where

  cellAvailable dest = availableToMoveTo stage dest

type Path = Array Point

type DistanceMap = Map.Map Point Int

wavePath :: Point -> Point -> (Point -> Boolean) -> Maybe Path
wavePath origin dest cellAvailable = iter [origin] 1 (Map.singleton origin 0)

  where

  iter :: Array Point -> Int -> DistanceMap -> Maybe Path
  iter toCheck step distanceMap = case unit of
    _ | null toCheck -> Nothing
    _ | any ((==) dest) newPoints -> Just (dest : buildPath dest step distanceMap)
    _ | otherwise -> iter newPoints (step + 1) newDistanceMap

    where

    newDistanceMap = foldr
      (\point -> Map.insert point step)
      distanceMap
      newPoints

    newPoints
      = filter (\point -> cellAvailable point && not (Map.member point distanceMap))
      $ nub -- Performance: Consider using sets
      $ concatMap adjustPoints toCheck

  buildPath :: Point -> Int -> DistanceMap -> Path
  buildPath pos step distanceMap = case find next (adjustPoints pos) of
    _ | step == 1 -> []
    Just p -> p : buildPath p (step - 1) distanceMap
    Nothing -> []

    where

    next :: Point -> Boolean
    next p = Map.lookup p distanceMap == Just (step - 1)
