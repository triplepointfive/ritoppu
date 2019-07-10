module Ritoppu.Action.CreatureAct
  ( creatureAct
  , wavePath
  ) where

import Prelude
import Prelude

import Data.Array (concatMap, filter, find, head, last, nub, null, (:))
import Data.Foldable (any, foldr)
import Data.Map (Map, delete, empty, insert, lookup, member, singleton, toUnfoldableUnordered) as Map
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Ritoppu.Action (Action(..), ActionResult, addAction, inactive, onResult)
import Ritoppu.Model (Creature, Game, Point, Stage, Stats, adjustPoints, anybodyAt, availableToMoveTo, creatureAt, creatureName, isNextTo)

-- TODO: Creatures must not to walk on each other
creatureAct :: Game -> ActionResult Game
creatureAct = actNext <<< inactive

actNext :: ActionResult Game -> ActionResult Game
actNext result@{ result: game } = case head creaturesToAct of
  Just creature -> actNext $ actCreature creature result
  Nothing -> result

  where

  creaturesToAct :: Array (Tuple Point Creature)
  creaturesToAct = filter
    (\(Tuple _ { turn }) -> game.stage.player.turn > turn)
    (Map.toUnfoldableUnordered game.stage.creatures)

actCreature :: Tuple Point Creature -> ActionResult Game -> ActionResult Game
actCreature (Tuple pos creature) result@{ result: game, actions }
  -- EXTRA: Ensure player is not dead yet
  | isNextTo pos game.stage.player.pos
    = addAction
      (LogMessage ("The " <> creatureName creature <> " insults you! Your ego is damaged!"))
      $ onResult (\game -> game
        { stage = moveCreature pos pos (acted creature) game.stage
        }) result
  | otherwise =
    onResult (\game -> game
      { stage = moveCreature pos (moveForward pos game.stage) (acted creature) game.stage
      }) result

acted :: Creature -> Creature
acted c = c { turn = c.turn + 5 }

addCreature :: Point -> Creature -> Stage -> Stage
addCreature pos creature stage =
  stage { creatures = Map.insert pos creature stage.creatures }

removeCreature :: Point -> Stage -> Stage
removeCreature pos stage =
  stage { creatures = Map.delete pos stage.creatures }

moveCreature :: Point -> Point -> Creature -> Stage -> Stage
moveCreature from dest creature = addCreature dest creature <<< removeCreature from

damage :: Stats -> Stats -> Int
damage { power } { defense } = abs (defense - power)

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
