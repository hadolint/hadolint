module Hadolint.Rule.DL3036Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3036 - Use BuildKit cache mount for zypper." $ do

    it "warn: zypper install without cache mount" $ do
      ruleCatches "DL3036" "RUN zypper install -y mariadb=10.4"
      onBuildRuleCatches "DL3036" "RUN zypper install -y mariadb=10.4"

    it "warn: zypper clean no longer suppresses" $ do
      ruleCatches "DL3036" "RUN zypper install -y mariadb=10.4 && zypper clean"
      ruleCatches "DL3036" "RUN zypper install -y mariadb=10.4 && zypper cc"
      onBuildRuleCatches "DL3036" "RUN zypper install -y mariadb=10.4 && zypper clean"
      onBuildRuleCatches "DL3036" "RUN zypper install -y mariadb=10.4 && zypper cc"

    it "warn: zypper install regardless of order" $ do
      ruleCatches "DL3036" "RUN zypper clean && zypper install -y mariadb=10.4"

    it "don't warn: cache mount at /var/cache/zypp" $
      let line = "RUN --mount=type=cache,target=/var/cache/zypp zypper install -y mariadb"
      in do
        ruleCatchesNot "DL3036" line
        onBuildRuleCatchesNot "DL3036" line

    it "don't warn: tmpfs mount at /var/cache/zypp" $
      let line = "RUN --mount=type=tmpfs,target=/var/cache/zypp zypper install -y mariadb"
      in do
        ruleCatchesNot "DL3036" line
        onBuildRuleCatchesNot "DL3036" line
