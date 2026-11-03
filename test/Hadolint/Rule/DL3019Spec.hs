module Hadolint.Rule.DL3019Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3019 - Use the --no-cache switch." $ do
    it "warn: apk add without --no-cache" $
      let line = "RUN apk add flex=2.6.4-r1"
      in do
        ruleCatches "DL3019" line
        onBuildRuleCatches "DL3019" line

    it "don't warn: apk add with --no-cache" $
      let line = "RUN apk add --no-cache flex=2.6.4-r1"
      in do
        ruleCatchesNot "DL3019" line
        onBuildRuleCatchesNot "DL3019" line

    it "don't warn: apk add with BuildKit cache mount" $
      let line = "RUN --mount=type=cache,target=/var/cache/apk apk add -U curl=7.77.0"
      in do
        ruleCatchesNot "DL3019" line
        onBuildRuleCatchesNot "DL3019" line

    it "don't warn: apk add with BuildKit tmpfs mount" $
      let line = "RUN --mount=type=tmpfs,target=/var/cache/apk apk add -U curl=7.77.0"
      in do
        ruleCatchesNot "DL3019" line
        onBuildRuleCatchesNot "DL3019" line

    it "don't warn: apk add with BuildKit cache mount in wrong dir and --no-cache" $
      let line = "RUN --mount=type=cache,target=/var/cache/foo apk add --no-cache -U curl=7.77.0"
      in do
        ruleCatchesNot "DL3019" line
        onBuildRuleCatchesNot "DL3019" line

    it "warn: apk add with BuildKit cache mount to wrong dir" $
      let line = "RUN --mount=type=cache,target=/var/cache/foo apk add -U curl=7.77.0"
      in do
        ruleCatches "DL3019" line
        onBuildRuleCatches "DL3019" line

    it "warn: apk add with BuildKit tmpfs mount to wrong dir" $
      let line = "RUN --mount=type=tmpfs,target=/var/cache/foo apk add -U curl=7.77.0"
      in do
        ruleCatches "DL3019" line
        onBuildRuleCatches "DL3019" line
