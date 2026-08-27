module Hadolint.Rule.DL3071Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3071 - Use BuildKit cache mount for Maven" $ do
    it "warn: mvn install without cache mount" $ do
      ruleCatches "DL3071" "RUN mvn install"
      onBuildRuleCatches "DL3071" "RUN mvn install"

    it "warn: mvn compile without cache mount" $ do
      ruleCatches "DL3071" "RUN mvn compile"
      onBuildRuleCatches "DL3071" "RUN mvn compile"

    it "don't warn: non-mvn command" $ do
      ruleCatchesNot "DL3071" "RUN echo hello"
      onBuildRuleCatchesNot "DL3071" "RUN echo hello"

    it "don't warn: cache mount at /root/.m2" $ do
      ruleCatchesNot "DL3071" "RUN --mount=type=cache,target=/root/.m2 mvn install"
      onBuildRuleCatchesNot "DL3071" "RUN --mount=type=cache,target=/root/.m2 mvn install"

    it "don't warn: tmpfs mount at /root/.m2" $ do
      ruleCatchesNot "DL3071" "RUN --mount=type=tmpfs,target=/root/.m2 mvn install"
      onBuildRuleCatchesNot "DL3071" "RUN --mount=type=tmpfs,target=/root/.m2 mvn install"

    it "warn: cache mount at wrong path" $ do
      ruleCatches "DL3071" "RUN --mount=type=cache,target=/wrong/path mvn install"
      onBuildRuleCatches "DL3071" "RUN --mount=type=cache,target=/wrong/path mvn install"
