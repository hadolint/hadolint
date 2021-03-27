module DL3014 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3014 - Use the -y switch." $ do
    it "apt-get auto yes" $ do
      ruleCatches "DL3014" "RUN apt-get install python"
      onBuildRuleCatches "DL3014" "RUN apt-get install python"
    it "apt-get yes shortflag" $ do
      ruleCatchesNot "DL3014" "RUN apt-get install -yq python"
      onBuildRuleCatchesNot "DL3014" "RUN apt-get install -yq python"
    it "apt-get yes quiet level 2 implies -y" $ do
      ruleCatchesNot "DL3014" "RUN apt-get install -qq python"
      onBuildRuleCatchesNot "DL3014" "RUN apt-get install -qq python"
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
