module Hadolint.Rule.DL3005Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3005" $ do
    it "apt-get dist-upgrade" $ do
      ruleCatches "DL3005" "RUN apt-get update && apt-get dist-upgrade"
      onBuildRuleCatches "DL3005" "RUN apt-get update && apt-get dist-upgrade"
