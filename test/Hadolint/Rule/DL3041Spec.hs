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

    it "not ok without dnf version pinning - package name with `-`" $ do
      ruleCatches "DL3041" "RUN dnf install -y rpm-sign && dnf clean all"
      ruleCatches "DL3041" "RUN microdnf install -y rpm-sign && microdnf clean all"
      onBuildRuleCatches "DL3041" "RUN dnf install -y rpm-sign && dnf clean all"

    it "ok with dnf version pinning" $ do
      ruleCatchesNot "DL3041" "RUN dnf install -y tomcat-9.0.1 && dnf clean all"
      ruleCatchesNot "DL3041" "RUN microdnf install -y tomcat-9.0.1 && microdnf clean all"
      onBuildRuleCatchesNot "DL3041" "RUN dnf install -y tomcat-9.0.1 && dnf clean all"
      onBuildRuleCatchesNot "DL3041" "RUN microdnf install -y tomcat-9.0.1 && microdnf clean all"

    it "ok with version pinning if command is not `dnf` or `microdnf`" $ do
      ruleCatchesNot "DL3041" "RUN notdnf install openssl-1:1.1.1k"
      onBuildRuleCatchesNot "DL3041" "RUN notdnf install openssl-1:1.1.1k"

    it "ok without version pinning if command is not `dnf` or `microdnf`" $ do
      ruleCatchesNot "DL3041" "RUN notdnf install tomcat"
      onBuildRuleCatchesNot "DL3041" "RUN notdnf install tomcat"

    it "ok with dnf version pinning - package name with `-`" $ do
      ruleCatchesNot "DL3041" "RUN dnf install -y rpm-sign-4.16.1.3 && dnf clean all"
      ruleCatchesNot "DL3041" "RUN microdnf install -y rpm-sign-4.16.1.3 && microdnf clean all"
      onBuildRuleCatchesNot "DL3041" "RUN dnf install -y rpm-sign-4.16.1.3 && dnf clean all"
      onBuildRuleCatchesNot "DL3041" "RUN microdnf install -y rpm-sign-4.16.1.3 && microdnf clean all"

    it "ok with dnf version pinning - package version with epoch" $ do
      ruleCatchesNot "DL3041" "RUN dnf install -y openssl-1:1.1.1k"
      ruleCatchesNot "DL3041" "RUN microdnf install -y openssl-1:1.1.1k"
      onBuildRuleCatchesNot "DL3041" "RUN dnf install -y openssl-1:1.1.1k"
      onBuildRuleCatchesNot "DL3041" "RUN microdnf install -y openssl-1:1.1.1k"

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
