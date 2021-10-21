module Hadolint.Rule.DL3015Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3015 - Avoid additional packages by specifying `--no-install-recommends`." $ do
    it "apt-get install recommends" $ do
      ruleCatchesNot
        "DL3015"
        "RUN apt-get install --no-install-recommends python"
      onBuildRuleCatchesNot
        "DL3015"
        "RUN apt-get install --no-install-recommends python"
    it "apt-get no install recommends" $ do
      ruleCatches "DL3015" "RUN apt-get install python"
      onBuildRuleCatches "DL3015" "RUN apt-get install python"
    it "apt-get no install recommends" $ do
      ruleCatches "DL3015" "RUN apt-get -y install python"
      onBuildRuleCatches "DL3015" "RUN apt-get -y install python"
    it "apt-get no install recommends via option" $ do
      ruleCatchesNot "DL3015" "RUN apt-get -o APT::Install-Recommends=false install python"
      onBuildRuleCatchesNot "DL3015" "RUN apt-get -o APT::Install-Recommends=false install python"
