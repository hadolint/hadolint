module Hadolint.Rule.DL3076Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3076 - Use BuildKit cache mount for Mix/Hex" $ do
    it "warn: mix deps.get without cache mount" $ do
      ruleCatches "DL3076" "RUN mix deps.get"
      onBuildRuleCatches "DL3076" "RUN mix deps.get"

    it "warn: mix compile without cache mount" $ do
      ruleCatches "DL3076" "RUN mix compile"
      onBuildRuleCatches "DL3076" "RUN mix compile"

    it "don't warn: non-mix command" $ do
      ruleCatchesNot "DL3076" "RUN echo hello"
      onBuildRuleCatchesNot "DL3076" "RUN echo hello"

    it "don't warn: cache mount at /root/.hex" $ do
      ruleCatchesNot "DL3076" "RUN --mount=type=cache,target=/root/.hex mix deps.get"
      onBuildRuleCatchesNot "DL3076" "RUN --mount=type=cache,target=/root/.hex mix deps.get"

    it "don't warn: cache mount at /root/.mix" $ do
      ruleCatchesNot "DL3076" "RUN --mount=type=cache,target=/root/.mix mix deps.get"
      onBuildRuleCatchesNot "DL3076" "RUN --mount=type=cache,target=/root/.mix mix deps.get"

    it "don't warn: tmpfs mount at /root/.hex" $ do
      ruleCatchesNot "DL3076" "RUN --mount=type=tmpfs,target=/root/.hex mix deps.get"
      onBuildRuleCatchesNot "DL3076" "RUN --mount=type=tmpfs,target=/root/.hex mix deps.get"

    it "warn: cache mount at wrong path" $ do
      ruleCatches "DL3076" "RUN --mount=type=cache,target=/wrong/path mix deps.get"
      onBuildRuleCatches "DL3076" "RUN --mount=type=cache,target=/wrong/path mix deps.get"
