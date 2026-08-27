module Hadolint.Rule.DL3032Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3032 - Use BuildKit cache mount for yum." $ do
    it "warn: yum install without cache mount" $ do
      ruleCatches "DL3032" "RUN yum install -y mariadb-10.4"
      onBuildRuleCatches "DL3032" "RUN yum install -y mariadb-10.4"

    it "warn: yum clean all no longer suppresses" $ do
      ruleCatches "DL3032" "RUN yum install -y mariadb-10.4 && yum clean all"
      onBuildRuleCatches "DL3032" "RUN yum install -y mariadb-10.4 && yum clean all"

    it "warn: rm -rf no longer suppresses" $ do
      ruleCatches "DL3032" "RUN yum install -y mariadb-10.4 && rm -rf /var/cache/yum/*"
      onBuildRuleCatches "DL3032" "RUN yum install -y mariadb-10.4 && rm -rf /var/cache/yum/*"

    it "don't warn: non-yum command" $ do
      ruleCatchesNot "DL3032" "RUN bash -c `# not even a yum command`"
      onBuildRuleCatchesNot "DL3032" "RUN bash -c `# not even a yum command`"

    it "warn: yum install regardless of order" $ do
      ruleCatches "DL3032" "RUN yum clean all && yum install -y mariadb-10.4"
      onBuildRuleCatches "DL3032" "RUN yum clean all && yum install -y mariadb-10.4"

    it "don't warn: cache mount at /var/cache/yum" $ do
      ruleCatchesNot "DL3032" "RUN --mount=type=cache,target=/var/cache/yum yum install -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3032" "RUN --mount=type=cache,target=/var/cache/yum yum install -y mariadb-10.4"

    it "don't warn: tmpfs mount at /var/cache/yum" $ do
      ruleCatchesNot "DL3032" "RUN --mount=type=tmpfs,target=/var/cache/yum yum install -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3032" "RUN --mount=type=tmpfs,target=/var/cache/yum yum install -y mariadb-10.4"
