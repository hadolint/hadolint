module Hadolint.Rule.DL3005Spec (spec) where

import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?rulesConfig = mempty
  describe "DL3005" $ do
    it "apt-get dist-upgrade" $ do
      ruleCatches "DL3005" "RUN apt-get update && apt-get dist-upgrade"
      onBuildRuleCatches "DL3005" "RUN apt-get update && apt-get dist-upgrade"
