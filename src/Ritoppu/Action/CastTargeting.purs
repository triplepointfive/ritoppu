module Ritoppu.Action.CastTargeting
  ( castFireball
  ) where

import Prelude

import Data.Array (filter, foldr)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Ritoppu.Action (Action(..), ActionResult, Message(..), addActions, inactive, withAction)
import Ritoppu.Action.CreatureAct (creatureAct)
import Ritoppu.Model (Creature, Game, Item(..), Point, doubleDistanceBetween, isVisibleTile, newCorpse)
import Ritoppu.Mutation (addItem, hitPlayer, removeCreature, removeItemFromInventory, takeDamage, updateCreature)

castFireball :: Point -> Game -> ActionResult Game
castFireball dest game = case isVisibleTile game.stage.fovMask dest of
  false -> withAction game (LogMessage TargetingOutOfFov)
  -- TODO: Check out of health
  true ->
    addActions (map LogMessage logs)
      $ creatureAct (playerTurn $ attackedStage $ attackPlayer game)

  where

  logs =
    [FireballExplodes radius]

    <>

    map (\(Tuple _ creature) -> BurnM creature damage) creatures

    <>

    if hitsPlayer then [BurnSelf damage] else []

  hitsPlayer = doubleDistanceBetween dest game.stage.player.pos <= radius * radius

  attackPlayer g = case hitsPlayer of
    true -> hitPlayer damage g
    false -> g

  -- TODO: remove duplicity
  attackedStage g = removeItem FireballScroll $ g { stage =
    foldr
      (\(Tuple pos creature) -> if creature.stats.hp <= damage
        then addItem pos (newCorpse creature) <<< removeCreature pos
        else updateCreature pos (\c -> c { stats = takeDamage damage c.stats }))
      g.stage
      creatures
  }

  -- TODO: Magic number
  radius = 3
  damage = 12

  -- EXTRA: Attack player as well
  creatures
    = filter (\(Tuple p _) -> doubleDistanceBetween p dest <= radius * radius)
    $ (Map.toUnfoldable game.stage.creatures :: Array (Tuple Point Creature))

-- TODO: Remove duplicity
playerTurn :: Game -> Game
playerTurn game = game { stage { player { turn = game.stage.player.turn + 5 } } }

-- TODO: Remove duplicity
removeItem :: Item -> Game -> Game
removeItem item game =
  game { stage { player { inventory = removeItemFromInventory item game.stage.player.inventory } } }
