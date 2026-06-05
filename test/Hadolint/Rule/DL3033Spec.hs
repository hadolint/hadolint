module Hadolint.Rule.DL3033Spec (spec) where

import qualified Data.Text as Text
import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3033 - Specify version with `yum install -y <package>-<version>`" $ do
    it "not ok without yum version pinning" $ do
      ruleCatches "DL3033" "RUN yum install -y tomcat && yum clean all"
      onBuildRuleCatches "DL3033" "RUN yum install -y tomcat && yum clean all"

    it "ok with yum version pinning" $ do
      ruleCatchesNot "DL3033" "RUN yum install -y tomcat-9.2 && yum clean all"
      ruleCatchesNot "DL3033" "RUN bash -c `# not even a yum command`"
      onBuildRuleCatchesNot "DL3033" "RUN yum install -y tomcat-9.2 && yum clean all"
      onBuildRuleCatchesNot "DL3033" "RUN bash -c `# not even a yum command`"

    it "ok with yum version pinning - version contains epoch" $ do
      ruleCatchesNot "DL3033" "RUN yum install -y openssl-1:1.1.1k"
      onBuildRuleCatchesNot "DL3033" "RUN yum install -y openssl-1:1.1.1k"

    it "ok with yum version pinning - package name contains `-`" $ do
      ruleCatchesNot "DL3033" "RUN yum install -y rpm-sign-4.16.1.3"
      onBuildRuleCatchesNot "DL3033" "RUN yum install -y rpm-sign-4.16.1.3"

    it "ok with yum version pinning - package name contains `-` and `+`" $ do
      ruleCatchesNot "DL3033" "RUN yum install -y gcc-c++-1.1.1"
      onBuildRuleCatchesNot "DL3033" "RUN yum install -y gcc-c++-1.1.1"

    it "not ok without yum version pinning - modules" $ do
      ruleCatches "DL3033" "RUN yum module install -y tomcat && yum clean all"
      onBuildRuleCatches "DL3033" "RUN yum module install -y tomcat && yum clean all"

    it "ok with yum version pinning - modules" $ do
      ruleCatchesNot "DL3033" "RUN yum module install -y tomcat:9 && yum clean all"
      ruleCatchesNot "DL3033" "RUN bash -c `# not even a yum command`"
      onBuildRuleCatchesNot "DL3033" "RUN yum module install -y tomcat:9 && yum clean all"
      onBuildRuleCatchesNot "DL3033" "RUN bash -c `# not even a yum command`"

    -- this is important e.g. when using renovatebot
    it "ok with version as variable - braced" $ do
      let
        rule = "DL3033"
        snippet =
          Text.unlines
            [ "ENV version=2.51.0-2.fc42",
              "RUN yum -y install git-core-${version}"
            ]
       in do
        ruleCatchesNot rule snippet

    it "ok with version as arg - unbraced" $ do
      let
        rule = "DL3033"
        snippet =
          Text.unlines
            [ "ARG version=2.51.0-2.fc42",
              "RUN yum -y install git-core-$version"
            ]
       in do
        ruleCatchesNot rule snippet

    it "ok with version as arg - different stages" $ do
      let
        rule = "DL3033"
        snippet =
          Text.unlines
            [ "FROM fedora:fc42 AS build",
              "ARG version=2.51.0-2.fc42",
              "FROM fedora:fc42",
              "RUN yum -y install git-core-${version}"
            ]
       in do
        ruleCatchesNot rule snippet

    it "ok with version as env - different stages, reused stage" $ do
      let
        rule = "DL3033"
        snippet =
          Text.unlines
            [ "FROM fedora:fc42 AS build",
              "ENV version=2.51.0-2.fc42",
              "FROM build",
              "RUN yum -y install git-core-${version}"
            ]
       in do
        ruleCatchesNot rule snippet

    it "not ok with version as variable - different stages, new stage" $ do
      let
        rule = "DL3033"
        snippet =
          Text.unlines
            [ "FROM fedora:fc42 AS build",
              "ENV version=2.51.0-2.fc42",
              "FROM fedora:fc42",
              "RUN yum -y install git-core-${version}"
            ]
       in do
        ruleCatches rule snippet
