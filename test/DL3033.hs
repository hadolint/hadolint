module DL3033 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3033 - Specify version with `yum install -y <package>-<version>`" $ do
    it "not ok wihout yum version pinning" $ do
      ruleCatches "DL3033" "RUN yum install -y tomcat && yum clean all"
      onBuildRuleCatches "DL3033" "RUN yum install -y tomcat && yum clean all"
    it "ok with yum version pinning" $ do
      ruleCatchesNot "DL3033" "RUN yum install -y tomcat-9.2 && yum clean all"
      ruleCatchesNot "DL3033" "RUN bash -c `# not even a yum command`"
      onBuildRuleCatchesNot "DL3033" "RUN yum install -y tomcat-9.2 && yum clean all"
      onBuildRuleCatchesNot "DL3033" "RUN bash -c `# not even a yum command`"
