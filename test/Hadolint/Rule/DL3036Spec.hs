module Hadolint.Rule.DL3036Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3036 - `zypper clean` missing after zypper use." $ do
    it "not ok without zypper clean" $ do
      ruleCatches "DL3036" "RUN zypper install -y mariadb=10.4"
      onBuildRuleCatches "DL3036" "RUN zypper install -y mariadb=10.4"
    it "ok with zypper clean" $ do
      ruleCatchesNot "DL3036" "RUN zypper install -y mariadb=10.4 && zypper clean"
      ruleCatchesNot "DL3036" "RUN zypper install -y mariadb=10.4 && zypper cc"
      onBuildRuleCatchesNot "DL3036" "RUN zypper install -y mariadb=10.4 && zypper clean"
      onBuildRuleCatchesNot "DL3036" "RUN zypper install -y mariadb=10.4 && zypper cc"
