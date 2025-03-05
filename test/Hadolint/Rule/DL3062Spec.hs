module Hadolint.Rule.DL3062Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def
  describe "DL3062 - Pin versions in pecl." $ do
    it "version pinned with arguments" $ do
      ruleCatchesNot "DL3062" "RUN pecl install redis-6.1.0 --phpize"
      onBuildRuleCatchesNot "DL3062" "RUN pecl install redis-6.1.0 --phpize"
    it "version pinned" $ do
      ruleCatchesNot "DL3062" "RUN pecl install redis-6.1.0"
      onBuildRuleCatchesNot "DL3062" "RUN pecl install redis-6.1.0"
    it "version pinned with channel" $ do
      ruleCatchesNot "DL3062" "RUN pecl install pecl.php.net/redis-6.1.0"
      onBuildRuleCatchesNot "DL3062" "RUN pecl.php.net/redis-6.1.0"
    it "pecl run install is fine" $ do
      ruleCatchesNot
        "DL3062"
        "RUN pecl run --crazy install"
      onBuildRuleCatchesNot
        "DL3062"
        "RUN pecl run --crazy install"

    it "version not pinned" $ do
      ruleCatches "DL3062" "RUN pecl install redis"
      onBuildRuleCatches "DL3062" "RUN pecl install redis"
    it "version not pinned with channel" $ do
      ruleCatches "DL3062" "RUN pecl install pecl.php.net/redis"
      onBuildRuleCatches "DL3062" "RUN pecl install pecl.php.net/redis"
    it "version not pinned multiple packages" $ do
      ruleCatches "DL3062" "RUN pecl install xdebug redis-6.1.0"
      onBuildRuleCatches "DL3062" "RUN pecl install xdebug redis-6.1.0"
    it "version not pinned multiple packages and channel" $ do
      ruleCatches "DL3062" "RUN pecl install pecl.php.net/xdebug redis-6.1.0"
      onBuildRuleCatches "DL3062" "RUN pecl install pecl.php.net/xdebug redis-6.1.0"
