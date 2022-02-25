module Hadolint.Rule.DL3038Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3038 - Use the `-y` switch to avoid manual input `dnf install -y <package>`" $ do
    it "not ok without dnf non-interactive flag" $ do
      ruleCatches "DL3038" "RUN dnf install httpd-2.4.24 && dnf clean all"
      ruleCatches "DL3038" "RUN microdnf install httpd-2.4.24 && microdnf clean all"
      onBuildRuleCatches "DL3038" "RUN dnf install httpd-2.4.24 && dnf clean all"
      onBuildRuleCatches "DL3038" "RUN microdnf install httpd-2.4.24 && microdnf clean all"
    it "ok with dnf non-interactive flag" $ do
      ruleCatchesNot "DL3038" "RUN dnf install -y httpd-2.4.24 && dnf clean all"
      ruleCatchesNot "DL3038" "RUN microdnf install -y httpd-2.4.24 && microdnf clean all"
      ruleCatchesNot "DL3038" "RUN notdnf install httpd"
      onBuildRuleCatchesNot "DL3038" "RUN dnf install -y httpd-2.4.24 && dnf clean all"
      onBuildRuleCatchesNot "DL3038" "RUN microdnf install -y httpd-2.4.24 && microdnf clean all"
      onBuildRuleCatchesNot "DL3038" "RUN notdnf install httpd"
