module Hadolint.Rule.DL3068Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3068 - Use BuildKit cache mount for npm" $ do
    it "warn: npm install without cache mount" $ do
      ruleCatches "DL3068" "RUN npm install"
      onBuildRuleCatches "DL3068" "RUN npm install"

    it "warn: npm ci without cache mount" $ do
      ruleCatches "DL3068" "RUN npm ci"
      onBuildRuleCatches "DL3068" "RUN npm ci"

    it "don't warn: non-npm command" $ do
      ruleCatchesNot "DL3068" "RUN echo hello"
      onBuildRuleCatchesNot "DL3068" "RUN echo hello"

    it "don't warn: cache mount at /root/.npm" $ do
      ruleCatchesNot "DL3068" "RUN --mount=type=cache,target=/root/.npm npm install"
      onBuildRuleCatchesNot "DL3068" "RUN --mount=type=cache,target=/root/.npm npm install"

    it "don't warn: tmpfs mount at /root/.npm" $ do
      ruleCatchesNot "DL3068" "RUN --mount=type=tmpfs,target=/root/.npm npm install"
      onBuildRuleCatchesNot "DL3068" "RUN --mount=type=tmpfs,target=/root/.npm npm install"

    it "warn: cache mount at wrong path" $ do
      ruleCatches "DL3068" "RUN --mount=type=cache,target=/wrong/path npm install"
      onBuildRuleCatches "DL3068" "RUN --mount=type=cache,target=/wrong/path npm install"

    it "don't warn: non-root user home path in mount (substring match)" $ do
      ruleCatchesNot "DL3068" "RUN --mount=type=cache,target=/home/node/.npm npm install"
      onBuildRuleCatchesNot "DL3068" "RUN --mount=type=cache,target=/home/node/.npm npm install"
