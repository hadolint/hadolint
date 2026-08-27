module Hadolint.Rule.DL3069Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3069 - Use BuildKit cache mount for cargo" $ do
    it "warn: cargo build without cache mount" $ do
      ruleCatches "DL3069" "RUN cargo build"
      onBuildRuleCatches "DL3069" "RUN cargo build"

    it "warn: cargo install without cache mount" $ do
      ruleCatches "DL3069" "RUN cargo install ripgrep"
      onBuildRuleCatches "DL3069" "RUN cargo install ripgrep"

    it "don't warn: non-cargo command" $ do
      ruleCatchesNot "DL3069" "RUN echo hello"
      onBuildRuleCatchesNot "DL3069" "RUN echo hello"

    it "don't warn: cache mount at /root/.cargo" $ do
      ruleCatchesNot "DL3069" "RUN --mount=type=cache,target=/root/.cargo cargo build"
      onBuildRuleCatchesNot "DL3069" "RUN --mount=type=cache,target=/root/.cargo cargo build"

    it "don't warn: both .cargo/registry and .cargo/git mounts present" $ do
      ruleCatchesNot "DL3069" "RUN --mount=type=cache,target=/root/.cargo/registry --mount=type=cache,target=/root/.cargo/git cargo build"
      onBuildRuleCatchesNot "DL3069" "RUN --mount=type=cache,target=/root/.cargo/registry --mount=type=cache,target=/root/.cargo/git cargo build"

    it "don't warn: tmpfs mount at /root/.cargo" $ do
      ruleCatchesNot "DL3069" "RUN --mount=type=tmpfs,target=/root/.cargo cargo build"
      onBuildRuleCatchesNot "DL3069" "RUN --mount=type=tmpfs,target=/root/.cargo cargo build"

    it "warn: cache mount at wrong path" $ do
      ruleCatches "DL3069" "RUN --mount=type=cache,target=/wrong/path cargo build"
      onBuildRuleCatches "DL3069" "RUN --mount=type=cache,target=/wrong/path cargo build"
