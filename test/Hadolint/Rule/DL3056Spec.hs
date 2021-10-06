module Hadolint.Rule.DL3056Spec (spec) where

import qualified Data.Map as Map
import qualified Hadolint.Process
import qualified Hadolint.Rule as Rule
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?rulesConfig = Hadolint.Process.RulesConfig
                      []
                      (Map.fromList [("semver", Rule.SemVer)])
                      (Just False)
  describe "DL3056 - Label `<label>` does not conform to semantic versioning." $ do
    it "not ok with label not containing semantic version" $ do
      ruleCatches "DL3056" "LABEL semver=\"not-sem-ver\""
      onBuildRuleCatches "DL3056" "LABEL semver=\"not-sem-ver\""
    it "ok with label containing semantic version" $ do
      ruleCatchesNot "DL3056" "LABEL semver=\"1.0.0\""
      onBuildRuleCatchesNot "DL3056" "LABEL semver=\"2.0.1-rc1\""
    it "ok with other label not containing semantic version" $ do
      ruleCatchesNot "DL3056" "LABEL other=\"foo\""
      onBuildRuleCatchesNot "DL3056" "LABEL other=\"bar\""
