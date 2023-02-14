module Hadolint.Rule.DL3033Spec (spec) where

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
    it "not ok without yum version pinning - modules" $ do
      ruleCatches "DL3033" "RUN yum module install -y tomcat && yum clean all"
      onBuildRuleCatches "DL3033" "RUN yum module install -y tomcat && yum clean all"
    it "ok with yum version pinning - modules" $ do
      ruleCatchesNot "DL3033" "RUN yum module install -y tomcat:9 && yum clean all"
      ruleCatchesNot "DL3033" "RUN bash -c `# not even a yum command`"
      onBuildRuleCatchesNot "DL3033" "RUN yum module install -y tomcat:9 && yum clean all"
      onBuildRuleCatchesNot "DL3033" "RUN bash -c `# not even a yum command`"
