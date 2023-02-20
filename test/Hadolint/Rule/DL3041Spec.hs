module Hadolint.Rule.DL3041Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3041 - Specify version with `dnf install -y <package>-<version>`" $ do
    it "not ok without dnf version pinning" $ do
      ruleCatches "DL3041" "RUN dnf install -y tomcat && dnf clean all"
      ruleCatches "DL3041" "RUN microdnf install -y tomcat && microdnf clean all"
      onBuildRuleCatches "DL3041" "RUN dnf install -y tomcat && dnf clean all"
    it "ok with dnf version pinning" $ do
      ruleCatchesNot "DL3041" "RUN dnf install -y tomcat-9.0.1 && dnf clean all"
      ruleCatchesNot "DL3041" "RUN microdnf install -y tomcat-9.0.1 && microdnf clean all"
      ruleCatchesNot "DL3041" "RUN notdnf install tomcat"
      onBuildRuleCatchesNot "DL3041" "RUN dnf install -y tomcat-9.0.1 && dnf clean all"
      onBuildRuleCatchesNot "DL3041" "RUN microdnf install -y tomcat-9.0.1 && microdnf clean all"
      onBuildRuleCatchesNot "DL3041" "RUN notdnf install tomcat"
    it "not ok without dnf version pinning - modules" $ do
      ruleCatches "DL3041" "RUN dnf module install -y tomcat && dnf clean all"
      ruleCatches "DL3041" "RUN microdnf module install -y tomcat && microdnf clean all"
      onBuildRuleCatches "DL3041" "RUN dnf module install -y tomcat && dnf clean all"
    it "ok with dnf version pinning - modules" $ do
      ruleCatchesNot "DL3041" "RUN dnf module install -y tomcat:9 && dnf clean all"
      ruleCatchesNot "DL3041" "RUN microdnf module install -y tomcat:9 && microdnf clean all"
      ruleCatchesNot "DL3041" "RUN notdnf module install tomcat"
      onBuildRuleCatchesNot "DL3041" "RUN dnf module install -y tomcat:9 && dnf clean all"
      onBuildRuleCatchesNot "DL3041" "RUN microdnf module install -y tomcat:9 && microdnf clean all"
      onBuildRuleCatchesNot "DL3041" "RUN notdnf module install tomcat"
