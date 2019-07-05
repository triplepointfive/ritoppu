module Ritoppu.Action.CreatureAct
  ( creatureAct
  , wavePath
  ) where

import Prelude

import Data.Array (concatMap, nub, null, filter, find, (:), last)
import Data.Foldable (any, foldr)
import Data.Map (Map, empty, insert, lookup, member, singleton, toUnfoldableUnordered) as Map
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Ritoppu.Action (ActionResult, inactive, onResult)
import Ritoppu.Model (Creature, Game, Point, Stage, Stats, adjustPoints, availableToMoveTo)

-- TODO: Creatures must not to walk on each other
creatureAct :: Game -> ActionResult Game
creatureAct game =
  foldr
      turn
      (inactive (game { stage { creatures = Map.empty :: Map.Map Point Creature } } ))
      ((Map.toUnfoldableUnordered game.stage.creatures) :: Array (Tuple Point Creature))

turn :: Tuple Point Creature -> ActionResult Game -> ActionResult Game
turn (Tuple pos creature) =
  onResult (\game -> game { stage = addCreature (moveForward pos game.stage) creature game.stage })

addCreature :: Point -> Creature -> Stage -> Stage
addCreature pos creature stage =
  stage { creatures = Map.insert pos creature stage.creatures }

damage :: Stats -> Stats -> Int
damage { power } { defense } = abs (defense - power)

-- TODO: Hit if next to player
moveForward :: Point -> Stage -> Point
moveForward origin stage@{ player: { pos } } =
  case wavePath origin pos cellAvailable of
      Just path -> case last path of
        Just dest -> dest
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
