module Hadolint.Rule.DL3072Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3072 - Use BuildKit cache mount for Gradle" $ do
    it "warn: gradle build without cache mount" $ do
      ruleCatches "DL3072" "RUN gradle build"
      onBuildRuleCatches "DL3072" "RUN gradle build"

    it "warn: ./gradlew build without cache mount" $ do
      ruleCatches "DL3072" "RUN ./gradlew build"
      onBuildRuleCatches "DL3072" "RUN ./gradlew build"

    it "don't warn: non-gradle command" $ do
      ruleCatchesNot "DL3072" "RUN echo hello"
      onBuildRuleCatchesNot "DL3072" "RUN echo hello"

    it "don't warn: cache mount at /root/.gradle" $ do
      ruleCatchesNot "DL3072" "RUN --mount=type=cache,target=/root/.gradle gradle build"
      onBuildRuleCatchesNot "DL3072" "RUN --mount=type=cache,target=/root/.gradle gradle build"

    it "don't warn: tmpfs mount at /root/.gradle" $ do
      ruleCatchesNot "DL3072" "RUN --mount=type=tmpfs,target=/root/.gradle gradle build"
      onBuildRuleCatchesNot "DL3072" "RUN --mount=type=tmpfs,target=/root/.gradle gradle build"

    it "don't warn: cache mount at /root/.gradle for ./gradlew" $ do
      ruleCatchesNot "DL3072" "RUN --mount=type=cache,target=/root/.gradle ./gradlew build"
      onBuildRuleCatchesNot "DL3072" "RUN --mount=type=cache,target=/root/.gradle ./gradlew build"

    it "warn: cache mount at wrong path" $ do
      ruleCatches "DL3072" "RUN --mount=type=cache,target=/wrong/path gradle build"
      onBuildRuleCatches "DL3072" "RUN --mount=type=cache,target=/wrong/path gradle build"
