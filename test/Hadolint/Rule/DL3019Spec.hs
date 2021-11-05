module Hadolint.Rule.DL3019Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3019 - Use the --no-cache switch." $ do
    it "apk add with --no-cache" $ do
      ruleCatches "DL3019" "RUN apk add flex=2.6.4-r1"
      onBuildRuleCatches "DL3019" "RUN apk add flex=2.6.4-r1"
    it "apk add without --no-cache" $ do
      ruleCatchesNot "DL3019" "RUN apk add --no-cache flex=2.6.4-r1"
      onBuildRuleCatchesNot "DL3019" "RUN apk add --no-cache flex=2.6.4-r1"
    it "don't warn: apk add with BuildKit cache mount" $ do
      ruleCatchesNot "DL3019" "RUN --mount=type=cache,target=/var/cache/apk apk add -U curl=7.77.0"
      onBuildRuleCatchesNot "DL3019" "RUN --mount=type=cache,target=/var/cache/apk apk add -U curl=7.77.0"
    it "don't warn: apk add with BuildKit cache mount in wrong dir and --no-cache" $ do
      ruleCatchesNot "DL3019" "RUN --mount=type=cache,target=/var/cache/foo apk add --no-cache -U curl=7.77.0"
      onBuildRuleCatchesNot "DL3019" "RUN --mount=type=cache,target=/var/cache/foo apk add --no-cache -U curl=7.77.0"
    it "warn: apk add with BuildKit cache mount to wrong dir" $ do
      ruleCatches "DL3019" "RUN --mount=type=cache,target=/var/cache/foo apk add -U curl=7.77.0"
      onBuildRuleCatches "DL3019" "RUN --mount=type=cache,target=/var/cache/foo apk add -U curl=7.77.0"
