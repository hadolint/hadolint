module Hadolint.Rule.DL3077Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3077 - Use BuildKit cache mount for gem/bundler" $ do
    it "warn: gem install without cache mount" $ do
      ruleCatches "DL3077" "RUN gem install rails"
      onBuildRuleCatches "DL3077" "RUN gem install rails"

    it "warn: bundle install without cache mount" $ do
      ruleCatches "DL3077" "RUN bundle install"
      onBuildRuleCatches "DL3077" "RUN bundle install"

    it "don't warn: non-gem/bundle command" $ do
      ruleCatchesNot "DL3077" "RUN echo hello"
      onBuildRuleCatchesNot "DL3077" "RUN echo hello"

    it "don't warn: cache mount at /usr/local/bundle" $ do
      ruleCatchesNot "DL3077" "RUN --mount=type=cache,target=/usr/local/bundle bundle install"
      onBuildRuleCatchesNot "DL3077" "RUN --mount=type=cache,target=/usr/local/bundle bundle install"

    it "don't warn: cache mount at path containing .gem" $ do
      ruleCatchesNot "DL3077" "RUN --mount=type=cache,target=/root/.gem gem install rails"
      onBuildRuleCatchesNot "DL3077" "RUN --mount=type=cache,target=/root/.gem gem install rails"

    it "don't warn: tmpfs mount at /usr/local/bundle" $ do
      ruleCatchesNot "DL3077" "RUN --mount=type=tmpfs,target=/usr/local/bundle bundle install"
      onBuildRuleCatchesNot "DL3077" "RUN --mount=type=tmpfs,target=/usr/local/bundle bundle install"

    it "warn: cache mount at wrong path" $ do
      ruleCatches "DL3077" "RUN --mount=type=cache,target=/wrong/path bundle install"
      onBuildRuleCatches "DL3077" "RUN --mount=type=cache,target=/wrong/path bundle install"
