module Hadolint.Rule.DL3034Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3034 - Non-interactive switch missing from zypper command: `zypper install -y`" $ do
    it "not ok without non-interactive switch" $ do
      ruleCatches "DL3034" "RUN zypper install httpd=2.4.24 && zypper clean"
      onBuildRuleCatches "DL3034" "RUN zypper install httpd=2.4.24 && zypper clean"
    it "ok with non-interactive switch present" $ do
      ruleCatchesNot "DL3034" "RUN zypper install -n httpd=2.4.24 && zypper clean"
      ruleCatchesNot "DL3034" "RUN zypper install --non-interactive httpd=2.4.24 && zypper clean"
      ruleCatchesNot "DL3034" "RUN zypper install -y httpd=2.4.24 && zypper clean"
      ruleCatchesNot "DL3034" "RUN zypper install --no-confirm httpd=2.4.24 && zypper clean"
      onBuildRuleCatchesNot "DL3034" "RUN zypper install -n httpd=2.4.24 && zypper clean"
      onBuildRuleCatchesNot "DL3034" "RUN zypper install --non-interactive httpd=2.4.24 && zypper clean"
      onBuildRuleCatchesNot "DL3034" "RUN zypper install -y httpd=2.4.24 && zypper clean"
      onBuildRuleCatchesNot "DL3034" "RUN zypper install --no-confirm httpd=2.4.24 && zypper clean"
