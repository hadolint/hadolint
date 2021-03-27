module DL3038 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3038 - Use the `-y` switch to avoid manual input `dnf install -y <package>`" $ do
    it "not ok without dnf non-interactive flag" $ do
      ruleCatches "DL3038" "RUN dnf install httpd-2.4.24 && dnf clean all"
      onBuildRuleCatches "DL3038" "RUN dnf install httpd-2.4.24 && dnf clean all"
    it "ok with dnf non-interactive flag" $ do
      ruleCatchesNot "DL3038" "RUN dnf install -y httpd-2.4.24 && dnf clean all"
      ruleCatchesNot "DL3038" "RUN notdnf install httpd"
      onBuildRuleCatchesNot "DL3038" "RUN dnf install -y httpd-2.4.24 && dnf clean all"
      onBuildRuleCatchesNot "DL3038" "RUN notdnf install httpd"
