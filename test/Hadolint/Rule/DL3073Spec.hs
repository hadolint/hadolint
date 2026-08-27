module Hadolint.Rule.DL3073Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3073 - Use BuildKit cache mount for Composer" $ do
    it "warn: composer install without cache mount" $ do
      ruleCatches "DL3073" "RUN composer install"
      onBuildRuleCatches "DL3073" "RUN composer install"

    it "warn: composer update without cache mount" $ do
      ruleCatches "DL3073" "RUN composer update"
      onBuildRuleCatches "DL3073" "RUN composer update"

    it "don't warn: non-composer command" $ do
      ruleCatchesNot "DL3073" "RUN echo hello"
      onBuildRuleCatchesNot "DL3073" "RUN echo hello"

    it "don't warn: cache mount at /root/.composer/cache" $ do
      ruleCatchesNot "DL3073" "RUN --mount=type=cache,target=/root/.composer/cache composer install"
      onBuildRuleCatchesNot "DL3073" "RUN --mount=type=cache,target=/root/.composer/cache composer install"

    it "don't warn: tmpfs mount at /root/.composer/cache" $ do
      ruleCatchesNot "DL3073" "RUN --mount=type=tmpfs,target=/root/.composer/cache composer install"
      onBuildRuleCatchesNot "DL3073" "RUN --mount=type=tmpfs,target=/root/.composer/cache composer install"

    it "warn: cache mount at wrong path" $ do
      ruleCatches "DL3073" "RUN --mount=type=cache,target=/wrong/path composer install"
      onBuildRuleCatches "DL3073" "RUN --mount=type=cache,target=/wrong/path composer install"
