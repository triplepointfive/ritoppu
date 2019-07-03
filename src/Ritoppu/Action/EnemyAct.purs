module Ritoppu.Action.EnemyAct
  ( creatureAct
  , wavePath
  , buildPath
  ) where

import Prelude
import Prelude

import Data.Array (concatMap, nub, null, filter, find, (:))
import Data.Foldable (any, foldr)
import Data.Int (round, toNumber)
import Data.Map as Map
import Data.Map as Map
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..), snd, uncurry)
import Math (sqrt)
import Pipes.Prelude as Map
import Ritoppu.Action (Action(..), ActionResult, addAction, inactive, onResult)
import Ritoppu.Model (Creature, Game, Point, Stage, adjustPoints, anybodyAt, availableToMoveTo, creatureName)

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

moveForward :: Point -> Stage -> Point
moveForward origin stage@{ player: { pos } } = case unit of
  _ | availableToMoveTo stage dest && dest /= pos && not (anybodyAt stage dest)
      -> dest
  _ -> origin
  where

  dest = origin +
      { x: round (dx / distance)
      , y: round (dy / distance)
      }

  dx = toNumber (pos.x - origin.x)
  dy = toNumber (pos.y - origin.y)

  distance = sqrt (dx * dx + dy * dy)

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
  Just p -> p : buildPath p (step - 1) distanceMap
  Nothing -> []

  where

  next :: Point -> Boolean
  next p = Map.lookup p distanceMap == Just (step - 1)
