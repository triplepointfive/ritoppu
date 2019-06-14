module Ritoppu.Model.Player
  ( Attributes
  , Inventory
  , Player
  , Race
  , Skills
  , Weapon
  , WeaponKind(..)
  ) where

import Prelude

import Data.Maybe (Maybe(..))

import Ritoppu.Model.Skill (Skill, skillValue)
import Ritoppu.Model.Attribute (Attribute, AttributeType(..), attributeValue)

type Attributes =
  { endurance :: Attribute
  , strength :: Attribute
  , agility :: Attribute
  , speed :: Attribute
  , personality :: Attribute
  , intelligence :: Attribute
  , willpower :: Attribute
  , luck :: Attribute
  }

type Skills =
  { heavyArmor :: Skill
  , mediumArmor :: Skill
  , spear :: Skill

  , acrobatics :: Skill
  , armorer :: Skill
  , axe :: Skill
  , bluntWeapon :: Skill
  , longBlade :: Skill

  , block :: Skill
  , lightArmor :: Skill
  , marksman :: Skill
  , sneak :: Skill

  , athletics :: Skill
  , handToHand :: Skill
  , shortBlade :: Skill
  , unarmored :: Skill

  , illusion :: Skill
  , mercantile :: Skill
  , speechcraft :: Skill

  , alchemy :: Skill
  , conjuration :: Skill
  , enchant :: Skill
  , security :: Skill

  , alteration :: Skill
  , destruction :: Skill
  , mysticism :: Skill
  , restoration :: Skill
  }

initSkills :: Skills
initSkills =
  { heavyArmor: newSkill Endurance
  , mediumArmor: newSkill Endurance
  , spear: newSkill Endurance

  , acrobatics: newSkill Strength
  , armorer: newSkill Strength
  , axe: newSkill Strength
  , bluntWeapon: newSkill Strength
  , longBlade: newSkill Strength

  , block: newSkill Agility
  , lightArmor: newSkill Agility
  , marksman: newSkill Agility
  , sneak: newSkill Agility

  , athletics: newSkill Speed
  , handToHand: newSkill Speed
  , shortBlade: newSkill Speed
  , unarmored: newSkill Speed

  , illusion: newSkill Personality
  , mercantile: newSkill Personality
  , speechcraft: newSkill Personality

  , alchemy: newSkill Intelligence
  , conjuration: newSkill Intelligence
  , enchant: newSkill Intelligence
  , security: newSkill Intelligence

  , alteration: newSkill Willpower
  , destruction: newSkill Willpower
  , mysticism: newSkill Willpower
  , restoration: newSkill Willpower
  }

  where

  newSkill :: AttributeType -> Skill
  newSkill governedBy = { own: 5, progress: 0.0, progressSpeed: 1.0, governedBy }

type Player =
  { attributes :: Attributes
  , skills :: Skills
  , race :: Race
  , inventory :: Inventory
  }

data WeaponKind
  = Axe
  | Blunt
  | Bow
  | Crossbow
  | LongBlade
  | ShortBlade
  | Spear
  | Throwing

 -- A value between min and max
type Weapon = { value :: Number, kind :: WeaponKind }

type Race =
  { handDamage :: Number
  }

type Inventory =
  { rightHand :: Maybe Weapon
  }

handToHandFatigueDamage :: Player -> Number -> Number
handToHandFatigueDamage player criticalHitModifier
  = skillValue player.skills.handToHand
  * player.race.handDamage
  / 2.0
  * criticalHitModifier

handToHandHealthDamage :: Player -> Number -> Number
handToHandHealthDamage player criticalHitModifier
  = skillValue player.skills.handToHand
  * player.race.handDamage
  * 0.075
  * criticalHitModifier

-- Current Fatigue / Maximum Fatigue
fatigueRate :: Player -> Number
fatigueRate player = 0.5

data Magnitude
  = Sanctuary
  | Blind
  | FortifyAttack

magnitude :: Player -> Magnitude -> Number
magnitude player kind = 0.0

weaponKindSkill :: Player -> WeaponKind -> Skill
weaponKindSkill { skills } = case _ of
  Axe -> skills.axe
  Blunt -> skills.bluntWeapon
  Bow -> skills.marksman
  Crossbow -> skills.marksman
  LongBlade -> skills.longBlade
  ShortBlade -> skills.shortBlade
  Spear -> skills.spear
  Throwing -> skills.marksman

weaponSkill :: Player -> Number
weaponSkill player = case player.inventory.rightHand of
  Just { kind } -> skillValue (weaponKindSkill player kind)
  Nothing -> skillValue player.skills.handToHand

hitRate :: Player -> Number
hitRate player =
  ( weaponSkill player
  + attributeValue player.attributes.agility / 5.0
  + attributeValue player.attributes.luck / 10.0
  )
  * (0.75 + 0.5 * fatigueRate player)
  + magnitude player FortifyAttack
  - magnitude player Blind

evasion :: Player -> Number
evasion player =
  ( attributeValue player.attributes.agility / 5.0
  + attributeValue player.attributes.luck / 10.0
  )
  * (0.75 + 0.5 * fatigueRate player)
  + magnitude player Sanctuary
