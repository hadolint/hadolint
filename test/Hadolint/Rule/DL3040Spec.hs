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
      onBuildRuleCatches "DL3040" "RUN dnf install -y mariadb-10.4"
      onBuildRuleCatches "DL3040" "RUN dnf install -y mariadb-10.4 && microdnf clean all"
      onBuildRuleCatches "DL3040" "RUN microdnf install -y mariadb-10.4"
    it "ok with dnf clean all" $ do
      ruleCatchesNot "DL3040" "RUN dnf install -y mariadb-10.4 && dnf clean all"
      ruleCatchesNot "DL3040" "RUN microdnf install -y mariadb-10.4 && microdnf clean all"
      ruleCatchesNot "DL3040" "RUN notdnf install mariadb"
      onBuildRuleCatchesNot "DL3040" "RUN dnf install -y mariadb-10.4 && dnf clean all"
      onBuildRuleCatchesNot "DL3040" "RUN microdnf install -y mariadb-10.4 && microdnf clean all"
      onBuildRuleCatchesNot "DL3040" "RUN notdnf install mariadb"
    it "ok with rm /var/cache/yum" $ do
      ruleCatchesNot "DL3040" "RUN dnf install -y mariadb-10.4 && rm -rf /var/cache/libdnf5"
      ruleCatchesNot "DL3040" "RUN microdnf install -y mariadb-10.4 && rm -rf /var/cache/libdnf5"
      onBuildRuleCatchesNot "DL3040" "RUN dnf install -y mariadb-10.4 && rm -rf /var/cache/libdnf5"
      onBuildRuleCatchesNot "DL3040" "RUN microdnf install -y mariadb-10.4 && rm -rf /var/cache/libdnf5"

    it "ok with cache mount at /var/cache/yum" $ do
      ruleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 dnf install -y mariadb-10.4"
      ruleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 microdnf install -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 dnf install -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3040" "RUN --mount=type=cache,target=/var/cache/libdnf5 microdnf install -y mariadb-10.4"
    it "ok with tmpfs mount at /var/cache/yum" $ do
      ruleCatchesNot "DL3040" "RUN --mount=type=tmpfs,target=/var/cache/libdnf5 dnf install -y mariadb-10.4"
      ruleCatchesNot "DL3040" "RUN --mount=type=tmpfs,target=/var/cache/libdnf5 microdnf install -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3040" "RUN --mount=type=tmpfs,target=/var/cache/libdnf5 dnf install -y mariadb-10.4"
      onBuildRuleCatchesNot "DL3040" "RUN --mount=type=tmpfs,target=/var/cache/libdnf5 microdnf install -y mariadb-10.4"
