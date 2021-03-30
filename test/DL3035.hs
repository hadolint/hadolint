module DL3035 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3035" $ do
    it "not ok: zypper update" $ do
      ruleCatches "DL3035" "RUN zypper update"
      ruleCatches "DL3035" "RUN zypper up"
      onBuildRuleCatches "DL3035" "RUN zypper update"
      onBuildRuleCatches "DL3035" "RUN zypper up"
    it "not ok: zypper dist-upgrade" $ do
      ruleCatches "DL3035" "RUN zypper dist-upgrade"
      ruleCatches "DL3035" "RUN zypper dup"
      onBuildRuleCatches "DL3035" "RUN zypper dist-upgrade"
      onBuildRuleCatches "DL3035" "RUN zypper dup"
