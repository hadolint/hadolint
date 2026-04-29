module Hadolint.Rule.DL3040Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3040 - `dnf clean all` missing after dnf command." $ do

    it "no ok without dnf clean all" $ do
      ruleCatches "DL3040" "RUN dnf install -y mariadb-10.4"
      ruleCatches "DL3040" "RUN microdnf install -y mariadb-10.4"
      ruleCatches "DL3040" "RUN dnf in -y mariadb-10.4"
      onBuildRuleCatches "DL3040" "RUN dnf install -y mariadb-10.4"
      onBuildRuleCatches "DL3040" "RUN dnf in -y mariadb-10.4"
      onBuildRuleCatches "DL3040" "RUN dnf install -y mariadb-10.4 && microdnf clean all"
      onBuildRuleCatches "DL3040" "RUN microdnf install -y mariadb-10.4"

    it "ok with dnf clean all" $ do
      ruleCatchesNot "DL3040" "RUN dnf install -y mariadb-10.4 && dnf clean all"
      ruleCatchesNot "DL3040" "RUN dnf in -y mariadb-10.4 && dnf clean all"
      ruleCatchesNot "DL3040" "RUN microdnf install -y mariadb-10.4 && microdnf clean all"
      ruleCatchesNot "DL3040" "RUN notdnf install mariadb"
      onBuildRuleCatchesNot "DL3040" "RUN dnf install -y mariadb-10.4 && dnf clean all"
      onBuildRuleCatchesNot "DL3040" "RUN microdnf install -y mariadb-10.4 && microdnf clean all"
      onBuildRuleCatchesNot "DL3040" "RUN notdnf install mariadb"

    it "ok with rm /var/cache/yum" $ do
      ruleCatchesNot "DL3040" "RUN dnf install -y mariadb-10.4 && rm -rf /var/cache/yum/*"
      ruleCatchesNot "DL3040" "RUN dnf in -y mariadb-10.4 && rm -rf /var/cache/yum/*"
      ruleCatchesNot "DL3040" "RUN microdnf install -y mariadb-10.4 && rm -rf /var/cache/yum/*"
      onBuildRuleCatchesNot "DL3040" "RUN dnf install -y mariadb-10.4 && rm -rf /var/cache/yum/*"
      onBuildRuleCatchesNot "DL3040" "RUN dnf in -y mariadb-10.4 && rm -rf /var/cache/yum/*"
      onBuildRuleCatchesNot "DL3040" "RUN microdnf install -y mariadb-10.4 && rm -rf /var/cache/yum/*"

    it "not ok with clean before install" $ do
      ruleCatches "DL3040" "RUN microdnf clean all && dnf install -y mariadb-10.4"
      ruleCatches "DL3040" "RUN microdnf clean all && dnf in -y mariadb-10.4"
      ruleCatches "DL3040" "RUN rm -rf /var/cache/libdnf5 && dnf install -y mariadb-10.4"
      ruleCatches "DL3040" "RUN rm -rf /var/cache/libdnf5 && dnf in -y mariadb-10.4"
      ruleCatches "DL3040" "RUN rm -rf /var/cache/libdnf5 && microdnf install -y mariadb-10.4"
      onBuildRuleCatches "DL3040" "RUN rm -rf /var/cache/libdnf5 && dnf install -y mariadb-10.4"
      onBuildRuleCatches "DL3040" "RUN rm -rf /var/cache/libdnf5 && dnf in -y mariadb-10.4"
      onBuildRuleCatches "DL3040" "RUN rm -rf /var/cache/libdnf5 && microdnf install -y mariadb-10.4"

    it "ok with cache mount at /var/cache/yum" $ do
      ruleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 dnf install -y mariadb-10.4"
      ruleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 dnf in -y mariadb-10.4"
      ruleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 microdnf install -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 dnf install -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 dnf in -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 microdnf install -y mariadb-10.4"

    it "ok with tmpfs mount at /var/cache/yum" $ do
      ruleCatchesNot "DL3040" "RUN --mount=type=tmpfs,target=/var/cache/libdnf5 dnf install -y mariadb-10.4"
      ruleCatchesNot "DL3040" "RUN --mount=type=tmpfs,target=/var/cache/libdnf5 dnf in -y mariadb-10.4"
      ruleCatchesNot "DL3040" "RUN --mount=type=tmpfs,target=/var/cache/libdnf5 microdnf install -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3040" "RUN --mount=type=tmpfs,target=/var/cache/libdnf5 dnf install -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3040" "RUN --mount=type=tmpfs,target=/var/cache/libdnf5 dnf in -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3040" "RUN --mount=type=tmpfs,target=/var/cache/libdnf5 microdnf install -y mariadb-10.4"

    it "not ok with `dnf upgrade`" $ do
      ruleCatches "DL3040" "RUN dnf -y upgrade"
      ruleCatches "DL3040" "RUN dnf -y up"
      onBuildRuleCatches "DL3040" "RUN dnf -y upgrade"
      onBuildRuleCatches "DL3040" "RUN dnf -y up"

    it "different install command variants" $ do
      ruleCatches "DL3040" "RUN dnf -y install"
      ruleCatches "DL3040" "RUN dnf -y in"
      ruleCatches "DL3040" "RUN dnf -y upgrade"
      ruleCatches "DL3040" "RUN dnf -y up"
      ruleCatches "DL3040" "RUN dnf -y upgrade-minimal"
      ruleCatches "DL3040" "RUN dnf -y up-min"
      ruleCatches "DL3040" "RUN dnf -y reinstall"
      ruleCatches "DL3040" "RUN dnf -y rei"
      ruleCatchesNot "DL3040" "RUN dnf -y install && dnf clean all"
      ruleCatchesNot "DL3040" "RUN dnf -y in && dnf clean all"
      ruleCatchesNot "DL3040" "RUN dnf -y upgrade && dnf clean all"
      ruleCatchesNot "DL3040" "RUN dnf -y up && dnf clean all"
      ruleCatchesNot "DL3040" "RUN dnf -y upgrade-minimal && dnf clean all"
      ruleCatchesNot "DL3040" "RUN dnf -y up-min && dnf clean all"
      ruleCatchesNot "DL3040" "RUN dnf -y reinstall && dnf clean all"
      ruleCatchesNot "DL3040" "RUN dnf -y rei && dnf clean all"
