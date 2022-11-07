module Hadolint.Rule.DL3032Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3032 - `yum clean all` missing after yum command." $ do
    it "not ok with no clean all" $ do
      ruleCatches "DL3032" "RUN yum install -y mariadb-10.4"
      onBuildRuleCatches "DL3032" "RUN yum install -y mariadb-10.4"
    it "ok with yum clean all " $ do
      ruleCatchesNot "DL3032" "RUN yum install -y mariadb-10.4 && yum clean all"
      ruleCatchesNot "DL3032" "RUN bash -c `# not even a yum command`"
      onBuildRuleCatchesNot "DL3032" "RUN yum install -y mariadb-10.4 && yum clean all"
      onBuildRuleCatchesNot "DL3032" "RUN bash -c `# not even a yum command`"
    it "ok with rm -rf /var/cache/yum/*" $ do
      ruleCatchesNot "DL3032" "RUN yum install -y mariadb-10.4 && rm -rf /var/cache/yum/*"
      onBuildRuleCatchesNot "DL3032" "RUN yum install -y mariadb-10.4 && rm -rf /var/cache/yum/*"
