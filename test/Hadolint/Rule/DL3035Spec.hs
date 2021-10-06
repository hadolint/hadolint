module Hadolint.Rule.DL3035Spec (spec) where

import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?rulesConfig = mempty
  describe "DL3035" $ do
    it "not ok: zypper dist-upgrade" $ do
      ruleCatches "DL3035" "RUN zypper dist-upgrade"
      ruleCatches "DL3035" "RUN zypper dup"
      onBuildRuleCatches "DL3035" "RUN zypper dist-upgrade"
      onBuildRuleCatches "DL3035" "RUN zypper dup"
