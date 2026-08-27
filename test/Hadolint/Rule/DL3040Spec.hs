module Hadolint.Rule.DL3040Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3040 - Use BuildKit cache mount for dnf." $ do

    it "warn: dnf install without cache mount" $ do
      ruleCatches "DL3040" "RUN dnf install -y mariadb-10.4"
      ruleCatches "DL3040" "RUN microdnf install -y mariadb-10.4"
      ruleCatches "DL3040" "RUN dnf in -y mariadb-10.4"
      onBuildRuleCatches "DL3040" "RUN dnf install -y mariadb-10.4"
      onBuildRuleCatches "DL3040" "RUN dnf in -y mariadb-10.4"
      onBuildRuleCatches "DL3040" "RUN microdnf install -y mariadb-10.4"

    it "warn: dnf clean all no longer suppresses" $ do
      ruleCatches "DL3040" "RUN dnf install -y mariadb-10.4 && dnf clean all"
      ruleCatches "DL3040" "RUN dnf in -y mariadb-10.4 && dnf clean all"
      ruleCatches "DL3040" "RUN microdnf install -y mariadb-10.4 && microdnf clean all"
      onBuildRuleCatches "DL3040" "RUN dnf install -y mariadb-10.4 && dnf clean all"
      onBuildRuleCatches "DL3040" "RUN microdnf install -y mariadb-10.4 && microdnf clean all"

    it "warn: rm -rf no longer suppresses" $ do
      ruleCatches "DL3040" "RUN dnf install -y mariadb-10.4 && rm -rf /var/cache/yum/*"
      ruleCatches "DL3040" "RUN dnf in -y mariadb-10.4 && rm -rf /var/cache/yum/*"
      ruleCatches "DL3040" "RUN microdnf install -y mariadb-10.4 && rm -rf /var/cache/yum/*"
      onBuildRuleCatches "DL3040" "RUN dnf install -y mariadb-10.4 && rm -rf /var/cache/yum/*"
      onBuildRuleCatches "DL3040" "RUN dnf in -y mariadb-10.4 && rm -rf /var/cache/yum/*"
      onBuildRuleCatches "DL3040" "RUN microdnf install -y mariadb-10.4 && rm -rf /var/cache/yum/*"

    it "don't warn: non-dnf command" $ do
      ruleCatchesNot "DL3040" "RUN notdnf install mariadb"
      onBuildRuleCatchesNot "DL3040" "RUN notdnf install mariadb"

    it "warn: dnf install regardless of order" $ do
      ruleCatches "DL3040" "RUN microdnf clean all && dnf install -y mariadb-10.4"
      ruleCatches "DL3040" "RUN rm -rf /var/cache/libdnf5 && dnf install -y mariadb-10.4"
      ruleCatches "DL3040" "RUN rm -rf /var/cache/libdnf5 && microdnf install -y mariadb-10.4"
      onBuildRuleCatches "DL3040" "RUN rm -rf /var/cache/libdnf5 && dnf install -y mariadb-10.4"
      onBuildRuleCatches "DL3040" "RUN rm -rf /var/cache/libdnf5 && microdnf install -y mariadb-10.4"

    it "don't warn: cache mount at /var/cache/libdnf5" $ do
      ruleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 dnf install -y mariadb-10.4"
      ruleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 dnf in -y mariadb-10.4"
      ruleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 microdnf install -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 dnf install -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 microdnf install -y mariadb-10.4"

    it "don't warn: tmpfs mount at /var/cache/libdnf5" $ do
      ruleCatchesNot "DL3040" "RUN --mount=type=tmpfs,target=/var/cache/libdnf5 dnf install -y mariadb-10.4"
      ruleCatchesNot "DL3040" "RUN --mount=type=tmpfs,target=/var/cache/libdnf5 microdnf install -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3040" "RUN --mount=type=tmpfs,target=/var/cache/libdnf5 dnf install -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3040" "RUN --mount=type=tmpfs,target=/var/cache/libdnf5 microdnf install -y mariadb-10.4"

    it "warn: dnf upgrade without cache mount" $ do
      ruleCatches "DL3040" "RUN dnf -y upgrade"
      ruleCatches "DL3040" "RUN dnf -y up"
      onBuildRuleCatches "DL3040" "RUN dnf -y upgrade"
      onBuildRuleCatches "DL3040" "RUN dnf -y up"

    it "warn: all install command variants without cache mount" $ do
      ruleCatches "DL3040" "RUN dnf -y install"
      ruleCatches "DL3040" "RUN dnf -y in"
      ruleCatches "DL3040" "RUN dnf -y upgrade"
      ruleCatches "DL3040" "RUN dnf -y up"
      ruleCatches "DL3040" "RUN dnf -y upgrade-minimal"
      ruleCatches "DL3040" "RUN dnf -y up-min"
      ruleCatches "DL3040" "RUN dnf -y reinstall"
      ruleCatches "DL3040" "RUN dnf -y rei"
