module Ritoppu.Model.SkillSpec
  ( spec
  ) where

import Prelude

import Ritoppu.Model (AttributeType(..))
import Ritoppu.Mutation.Skill (practice)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "practice" do
    describe "when max" do
      let skill = { own: 100, progress: 100.0, progressSpeed: 1.0, governedBy: Luck }
      it "does not change" do
        practice skill `shouldEqual` skill
    describe "close to max" do
      let skill = { own: 99, progress: 99.0, progressSpeed: 3.0, governedBy: Luck }
      it "increases to max" do
        practice skill `shouldEqual` { own: 100, progress: 100.0, progressSpeed: 0.0, governedBy: Luck }
    describe "middle own level" do
      let skill = { own: 10, progress: 99.0, progressSpeed: 3.0, governedBy: Luck }
      it "levels up" do
        practice skill `shouldEqual` { own: 11, progress: 0.0, progressSpeed: 3.0, governedBy: Luck }
    describe "progress middle" do
      let skill = { own: 10, progress: 30.0, progressSpeed: 3.0, governedBy: Luck }
      it "levels up" do
        practice skill `shouldEqual` { own: 10, progress: 33.0, progressSpeed: 3.0, governedBy: Luck }
