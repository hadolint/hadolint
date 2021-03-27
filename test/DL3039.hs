module DL3039 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3039" $ do
    it "dnf update" $ do
      ruleCatches "DL3039" "RUN dnf upgrade"
      ruleCatches "DL3039" "RUN dnf upgrade-minimal"
      onBuildRuleCatches "DL3039" "RUN dnf upgrade"
      onBuildRuleCatches "DL3039" "RUN dnf upgrade-minimal"
    it "not dnf upgrade" $ do
      ruleCatchesNot "DL3039" "RUN notdnf upgrade"
      onBuildRuleCatchesNot "DL3039" "RUN notdnf upgrade"
