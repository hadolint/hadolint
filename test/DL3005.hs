module DL3005 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3005" $ do
    it "apt-get dist-upgrade" $ do
      ruleCatches "DL3005" "RUN apt-get update && apt-get dist-upgrade"
      onBuildRuleCatches "DL3005" "RUN apt-get update && apt-get dist-upgrade"
