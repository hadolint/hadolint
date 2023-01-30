module Hadolint.Rule.DL3014Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3014 - Use the -y switch." $ do
    it "apt-get auto yes" $ do
      ruleCatches "DL3014" "RUN apt-get install python"
      onBuildRuleCatches "DL3014" "RUN apt-get install python"

    describe "Insufficient Quiet Levels" $ do
      it "apt-get -q" $ do
        ruleCatches "DL3014" "RUN apt-get install -q python"
        onBuildRuleCatches "DL3014" "RUN apt-get install -q python"
      it "apt-get --quiet" $ do
        ruleCatches "DL3014" "RUN apt-get install --quiet python"
        onBuildRuleCatches "DL3014" "RUN apt-get install -q python"

    it "apt-get yes shortflag" $ do
      ruleCatchesNot "DL3014" "RUN apt-get install -yq python"
      onBuildRuleCatchesNot "DL3014" "RUN apt-get install -yq python"
    it "apt-get yes different pos" $ do
      ruleCatchesNot "DL3014" "RUN apt-get install -y python"
      onBuildRuleCatchesNot "DL3014" "RUN apt-get install -y python"
    it "apt-get with auto yes" $ do
      ruleCatchesNot "DL3014" "RUN apt-get -y install python"
      onBuildRuleCatchesNot "DL3014" "RUN apt-get -y install python"
    it "apt-get with auto expanded yes" $ do
      ruleCatchesNot "DL3014" "RUN apt-get --yes install python"
      onBuildRuleCatchesNot "DL3014" "RUN apt-get --yes install python"
    it "apt-get with assume-yes" $ do
      ruleCatchesNot "DL3014" "RUN apt-get --assume-yes install python"
      onBuildRuleCatchesNot "DL3014" "RUN apt-get --assume-yes install python"

    describe "Quiet level 2 implies assume-yes" $ do
      it "apt-get -qq" $ do
        ruleCatchesNot "DL3014" "RUN apt-get install -qq python"
        onBuildRuleCatchesNot "DL3014" "RUN apt-get install -qq python"
      it "apt-get -q -q" $ do
        ruleCatchesNot "DL3014" "RUN apt-get install -q -q python"
        onBuildRuleCatchesNot "DL3014" "RUN apt-get install -q -q python"
      it "apt-get -q=2" $ do
        ruleCatchesNot "DL3014" "RUN apt-get install -q=2 python"
        onBuildRuleCatchesNot "DL3014" "RUN apt-get install -q=2 python"
      it "apt-get --quiet --quiet" $ do
        ruleCatchesNot "DL3014" "RUN apt-get install --quiet --quiet python"
        onBuildRuleCatchesNot "DL3014"
          "RUN apt-get install --quiet --quiet python"
