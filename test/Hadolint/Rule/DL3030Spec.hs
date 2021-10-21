module Hadolint.Rule.DL3030Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def
  describe "DL3030 - Use the `-y` switch to avoid manual input `yum install -y <package>`" $ do
    it "not ok when not using `-y` switch" $ do
      ruleCatches "DL3030" "RUN yum install httpd-2.4.24 && yum clean all"
      onBuildRuleCatches "DL3030" "RUN yum install httpd-2.4.24 && yum clean all"
    it "ok when using `-y` switch" $ do
      ruleCatchesNot "DL3030" "RUN yum install -y httpd-2.4.24 && yum clean all"
      ruleCatchesNot "DL3030" "RUN bash -c `# not even a yum command`"
      onBuildRuleCatchesNot "DL3030" "RUN yum install -y httpd-2.4.24 && yum clean all"
      onBuildRuleCatchesNot "DL3030" "RUN bash -c `# not even a yum command`"
