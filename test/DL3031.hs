module DL3031 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3031" $ do
    it "yum update" $ do
      ruleCatches "DL3031" "RUN yum update"
      onBuildRuleCatches "DL3031" "RUN yum update"
    it "not yum update" $ do
      ruleCatchesNot "DL3031" "RUN yum install -y httpd-2.4.42 && yum clean all"
      ruleCatchesNot "DL3031" "RUN bash -c `# not even a yum command`"
      onBuildRuleCatchesNot "DL3031" "RUN yum install -y httpd-2.4.42 && yum clean all"
      onBuildRuleCatchesNot "DL3031" "RUN bash -c `# not even a yum command`"
