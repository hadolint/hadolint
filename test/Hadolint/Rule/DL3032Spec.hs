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
    it "not ok with clean before install" $ do
      ruleCatchesNot "DL3032" "RUN yum install -y mariadb-10.4 && yum clean all"
      ruleCatches "DL3032" "RUN yum clean all && yum install -y"

    it "strict all-args matching for yum cleanup" $ do
      ruleCatches "DL3032" "RUN yum install -y bash && yum clean"
      ruleCatches "DL3032" "RUN yum install -y bash && yum clean cache"
      ruleCatchesNot "DL3032" "RUN yum install -y bash && yum clean all"
      ruleCatches "DL3032" "RUN yum install -y bash && rm -rf /var/lib/apt/lists/*"
      ruleCatchesNot "DL3032" "RUN yum install -y bash && rm -rf /var/cache/yum/*"
      ruleCatches "DL3032" "RUN yum install -y bash && rm -rf /var/cache/libdnf5"
      ruleCatchesNot "DL3032" "RUN rm -rf /foo/bar && yum install -y bash && rm -rf /var/cache/yum/*"
      onBuildRuleCatches "DL3032" "RUN yum install -y bash && yum clean"
      onBuildRuleCatches "DL3032" "RUN yum install -y bash && yum clean cache"
      onBuildRuleCatchesNot "DL3032" "RUN yum install -y bash && yum clean all"
      onBuildRuleCatches "DL3032" "RUN yum install -y bash && rm -rf /var/lib/apt/lists/*"
      onBuildRuleCatchesNot "DL3032" "RUN yum install -y bash && rm -rf /var/cache/yum/*"
      onBuildRuleCatches "DL3032" "RUN yum install -y bash && rm -rf /var/cache/libdnf5"
      onBuildRuleCatchesNot "DL3032" "RUN rm -rf /foo/bar && yum install -y bash && rm -rf /var/cache/yum/*"
