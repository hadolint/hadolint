module Hadolint.Rule.DL3068Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def
  describe "DL3068 - Pin versions in pecl." $ do
    describe "pinned versions are accepted" $ do
      it "version pinned" $ do
        ruleCatchesNot "DL3068" "RUN pecl install redis-6.1.0"
        onBuildRuleCatchesNot "DL3068" "RUN pecl install redis-6.1.0"
      it "version pinned with arguments" $ do
        ruleCatchesNot "DL3068" "RUN pecl install redis-6.1.0 --phpize"
        onBuildRuleCatchesNot "DL3068" "RUN pecl install redis-6.1.0 --phpize"
      it "version pinned with a leading force flag" $ do
        ruleCatchesNot "DL3068" "RUN pecl install -f redis-6.1.0"
        onBuildRuleCatchesNot "DL3068" "RUN pecl install -f redis-6.1.0"
      it "single-digit version pinned" $ do
        ruleCatchesNot "DL3068" "RUN pecl install redis-6"
        onBuildRuleCatchesNot "DL3068" "RUN pecl install redis-6"
      it "package name containing an underscore, pinned" $ do
        ruleCatchesNot "DL3068" "RUN pecl install pecl_http-4.2.4"
        onBuildRuleCatchesNot "DL3068" "RUN pecl install pecl_http-4.2.4"

    describe "channel-qualified names" $ do
      it "version pinned with channel" $ do
        ruleCatchesNot "DL3068" "RUN pecl install pecl.php.net/redis-6.1.0"
        onBuildRuleCatchesNot "DL3068" "RUN pecl install pecl.php.net/redis-6.1.0"
      it "version pinned with a short channel alias" $ do
        ruleCatchesNot "DL3068" "RUN pecl install mychannel/redis-6.1.0"
        onBuildRuleCatchesNot "DL3068" "RUN pecl install mychannel/redis-6.1.0"
      it "version pinned with a channel URI" $ do
        ruleCatchesNot "DL3068" "RUN pecl install channel://pecl.php.net/redis-6.1.0"
        onBuildRuleCatchesNot "DL3068" "RUN pecl install channel://pecl.php.net/redis-6.1.0"
      it "version not pinned with channel" $ do
        ruleCatches "DL3068" "RUN pecl install pecl.php.net/redis"
        onBuildRuleCatches "DL3068" "RUN pecl install pecl.php.net/redis"
      it "version not pinned with a channel URI" $ do
        ruleCatches "DL3068" "RUN pecl install channel://pecl.php.net/redis"
        onBuildRuleCatches "DL3068" "RUN pecl install channel://pecl.php.net/redis"

    describe "already-fixed artifacts are accepted" $ do
      it "local tgz artifact" $ do
        ruleCatchesNot "DL3068" "RUN pecl install redis-6.1.0.tgz"
        onBuildRuleCatchesNot "DL3068" "RUN pecl install redis-6.1.0.tgz"
      it "local tgz artifact with a relative path" $ do
        ruleCatchesNot "DL3068" "RUN pecl install ./build/redis-6.1.0.tgz"
        onBuildRuleCatchesNot "DL3068" "RUN pecl install ./build/redis-6.1.0.tgz"
      it "local tgz artifact with an absolute path" $ do
        ruleCatchesNot "DL3068" "RUN pecl install /tmp/redis-6.1.0.tgz"
        onBuildRuleCatchesNot "DL3068" "RUN pecl install /tmp/redis-6.1.0.tgz"
      it "local package.xml descriptor" $ do
        ruleCatchesNot "DL3068" "RUN pecl install package.xml"
        onBuildRuleCatchesNot "DL3068" "RUN pecl install package.xml"
      it "local package.xml descriptor with a path" $ do
        ruleCatchesNot "DL3068" "RUN pecl install /src/ext/package.xml"
        onBuildRuleCatchesNot "DL3068" "RUN pecl install /src/ext/package.xml"
      it "remote https download" $ do
        ruleCatchesNot "DL3068" "RUN pecl install https://example.com/redis.tgz"
        onBuildRuleCatchesNot "DL3068" "RUN pecl install https://example.com/redis.tgz"

    describe "unpinned installs are flagged" $ do
      it "version not pinned" $ do
        ruleCatches "DL3068" "RUN pecl install redis"
        onBuildRuleCatches "DL3068" "RUN pecl install redis"
      it "package name with underscore, not pinned" $ do
        ruleCatches "DL3068" "RUN pecl install pecl_http"
        onBuildRuleCatches "DL3068" "RUN pecl install pecl_http"
      it "version not pinned multiple packages" $ do
        ruleCatches "DL3068" "RUN pecl install xdebug redis-6.1.0"
        onBuildRuleCatches "DL3068" "RUN pecl install xdebug redis-6.1.0"
      it "version not pinned multiple packages and channel" $ do
        ruleCatches "DL3068" "RUN pecl install pecl.php.net/xdebug redis-6.1.0"
        onBuildRuleCatches "DL3068" "RUN pecl install pecl.php.net/xdebug redis-6.1.0"

    describe "floating stability keywords are flagged" $ do
      it "beta state not pinned" $ do
        ruleCatches "DL3068" "RUN pecl install redis-beta"
        onBuildRuleCatches "DL3068" "RUN pecl install redis-beta"
      it "alpha state not pinned" $ do
        ruleCatches "DL3068" "RUN pecl install xdebug-alpha"
        onBuildRuleCatches "DL3068" "RUN pecl install xdebug-alpha"
      it "stable state not pinned" $ do
        ruleCatches "DL3068" "RUN pecl install redis-stable"
        onBuildRuleCatches "DL3068" "RUN pecl install redis-stable"

    describe "pecl subcommands other than install are ignored" $ do
      it "pecl run install is fine" $ do
        ruleCatchesNot "DL3068" "RUN pecl run --crazy install"
        onBuildRuleCatchesNot "DL3068" "RUN pecl run --crazy install"
