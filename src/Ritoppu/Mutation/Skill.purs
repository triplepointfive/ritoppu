module Ritoppu.Mutation.Skill
  ( practice
  ) where

import Prelude

import Ritoppu.Model (Skill)

practice :: Skill -> Skill
practice skill = case unit of
  _ | skill.own == 100
      -> skill
  _ | newProgress >= 100.0 && skill.own == 99
      -> skill { own = 100, progress = 100.0, progressSpeed = 0.0 }
  _ | newProgress >= 100.0
      -> skill { own = skill.own + 1, progress = 0.0 }
  _ -> skill { progress = skill.progress + skill.progressSpeed }

  where

  newProgress = skill.progress + skill.progressSpeed
