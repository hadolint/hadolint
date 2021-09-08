module DL3041 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3041 - Specify version with `dnf install -y <package>-<version>`" $ do
    it "not ok without dnf version pinning" $ do
      ruleCatches "DL3041" "RUN dnf install -y tomcat && dnf clean all"
      onBuildRuleCatches "DL3041" "RUN dnf install -y tomcat && dnf clean all"
    it "ok with dnf version pinning" $ do
      ruleCatchesNot "DL3041" "RUN dnf install -y tomcat-9.0.1 && dnf clean all"
      ruleCatchesNot "DL3041" "RUN notdnf install tomcat"
      onBuildRuleCatchesNot "DL3041" "RUN dnf install -y tomcat-9.0.1 && dnf clean all"
      onBuildRuleCatchesNot "DL3041" "RUN notdnf install tomcat"
    it "not ok without dnf version pinning - moudles" $ do
      ruleCatches "DL3041" "RUN dnf module install -y tomcat && dnf clean all"
      onBuildRuleCatches "DL3041" "RUN dnf module install -y tomcat && dnf clean all"
    it "ok with dnf version pinning - modules" $ do
      ruleCatchesNot "DL3041" "RUN dnf module install -y tomcat:9 && dnf clean all"
      ruleCatchesNot "DL3041" "RUN notdnf module install tomcat"
      onBuildRuleCatchesNot "DL3041" "RUN dnf module install -y tomcat:9 && dnf clean all"
      onBuildRuleCatchesNot "DL3041" "RUN notdnf module install tomcat"
