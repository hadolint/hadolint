module Hadolint.Rule.DL3075Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3075 - Use BuildKit cache mount for pnpm" $ do
    it "warn: pnpm install without cache mount" $ do
      ruleCatches "DL3075" "RUN pnpm install"
      onBuildRuleCatches "DL3075" "RUN pnpm install"

    it "warn: pnpm add without cache mount" $ do
      ruleCatches "DL3075" "RUN pnpm add lodash"
      onBuildRuleCatches "DL3075" "RUN pnpm add lodash"

    it "don't warn: non-pnpm command" $ do
      ruleCatchesNot "DL3075" "RUN echo hello"
      onBuildRuleCatchesNot "DL3075" "RUN echo hello"

    it "don't warn: cache mount at /root/.local/share/pnpm/store" $ do
      ruleCatchesNot "DL3075" "RUN --mount=type=cache,target=/root/.local/share/pnpm/store pnpm install"
      onBuildRuleCatchesNot "DL3075" "RUN --mount=type=cache,target=/root/.local/share/pnpm/store pnpm install"

    it "don't warn: tmpfs mount at /root/.local/share/pnpm/store" $ do
      ruleCatchesNot "DL3075" "RUN --mount=type=tmpfs,target=/root/.local/share/pnpm/store pnpm install"
      onBuildRuleCatchesNot "DL3075" "RUN --mount=type=tmpfs,target=/root/.local/share/pnpm/store pnpm install"

    it "warn: cache mount at wrong path" $ do
      ruleCatches "DL3075" "RUN --mount=type=cache,target=/wrong/path pnpm install"
      onBuildRuleCatches "DL3075" "RUN --mount=type=cache,target=/wrong/path pnpm install"
