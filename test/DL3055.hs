module DL3055 (tests) where

import qualified Data.Map as Map
import qualified Hadolint.Process
import qualified Hadolint.Rule as Rule
import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = Hadolint.Process.RulesConfig [] (Map.fromList [("githash", Rule.GitHash)]) False
  describe "DL3055 - Label `<label>` is not a valid git hash." $ do
    it "not ok with label not containing git hash" $ do
      ruleCatches "DL3055" "LABEL githash=\"not-git-hash\""
      onBuildRuleCatches "DL3055" "LABEL githash=\"not-git-hash\""
    it "ok with label containing short git hash" $ do
      ruleCatchesNot "DL3055" "LABEL githash=\"2dbfae9\""
      onBuildRuleCatchesNot "DL3055" "LABEL githash=\"2dbfae9\""
    it "ok with label containing long git hash" $ do
      ruleCatchesNot "DL3055" "LABEL githash=\"43c572f1272b6b3171dd1db9e41b7027128ce080\""
      onBuildRuleCatchesNot "DL3055" "LABEL githash=\"43c572f1272b6b3171dd1db9e41b7027128ce080\""
    it "ok with other label not containing git hash" $ do
      ruleCatchesNot "DL3055" "LABEL other=\"foo\""
      onBuildRuleCatchesNot "DL3055" "LABEL other=\"bar\""
