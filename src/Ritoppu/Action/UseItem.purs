module Ritoppu.Action.UseItem
  ( useItem
  ) where

import Prelude

import Data.Array (filter)
import Data.Foldable (minimumBy)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Ritoppu.Action (Action(..), ActionResult, Message(..), addAction, die, inactive, target, withAction)
import Ritoppu.Action.CastTargeting (castFireball, castConfusion)
import Ritoppu.Action.CreatureAct (creatureAct)
import Ritoppu.Model (Creature, Game, Item(..), Point, doubleDistanceBetween, isFullHealth, newCorpse)
import Ritoppu.Mutation (addItem, heal, removeCreature, removeItemFromInventory, takeDamage, updateCreature, hitPlayer)

useItem :: Item -> Game -> ActionResult Game
useItem = case _ of
  HealingPotion -> useHealingPotion
  LightningScroll -> useLightningScroll
  FireballScroll -> target castFireball "Left-click a target tile for the fireball" <<< inactive
  ConfusionScroll -> target castConfusion "Left-click an enemy to confuse it" <<< inactive
  Corpse _ -> flip withAction (LogMessage DoNotKnowHowToUse)

useHealingPotion :: Game -> ActionResult Game
useHealingPotion game = case unit of
  _ | isFullHealth game.stage.player.stats ->
    withAction game (LogMessage FullHealth)
  _
    -> addAction (LogMessage Healed)
    -- TODO: Magic number
    $ creatureAct (removeItem HealingPotion $ playerTurn game { stage { player { stats = heal 4 game.stage.player.stats } } } )

-- TODO: Remove duplicity
playerTurn :: Game -> Game
playerTurn game = game { stage { player { turn = game.stage.player.turn + 5 } } }

-- TODO: Remove duplicity
removeItem :: Item -> Game -> Game
removeItem item game =
  game { stage { player { inventory = removeItemFromInventory item game.stage.player.inventory } } }

-- TODO: Better messages
useLightningScroll :: Game -> ActionResult Game
useLightningScroll game = case target of
  Just (Tuple pos creature) | damage >= creature.stats.hp ->
    addAction (LogMessage (AttackKillM creature))
      $ creatureAct $ actedGame { stage = addItem pos (newCorpse creature) $ removeCreature pos actedGame.stage }
  Just (Tuple pos creature) ->
    addAction (LogMessage (AttackM creature damage))
      $ creatureAct $ actedGame
        { stage = updateCreature pos (\c -> c { stats = takeDamage damage c.stats }) game.stage
        }
  Nothing | damage >= game.stage.player.stats.hp ->
    die $ withAction (hitPlayer damage actedGame) (LogMessage LightningScrollHitYourself)
  Nothing ->
    addAction
      (LogMessage LightningScrollHitYourself)
      $ creatureAct
      $ hitPlayer damage actedGame

  where

  actedGame = removeItem LightningScroll $ playerTurn game

  pPos = game.stage.player.pos

  target :: Maybe (Tuple Point Creature)
  target
    = minimumBy (\(Tuple p1 _) (Tuple p2 _) -> doubleDistanceBetween p1 pPos `compare` doubleDistanceBetween p2 pPos)
    $ filter (\(Tuple p _) -> doubleDistanceBetween p pPos <= 25) -- TODO: Magic number
    $ (Map.toUnfoldable game.stage.creatures :: Array (Tuple Point Creature))
  damage = 20
