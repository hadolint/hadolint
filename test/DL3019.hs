module DL3019 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3019 - Use the --no-cache switch." $ do
    it "apk add with --no-cache" $ do
      ruleCatches "DL3019" "RUN apk add flex=2.6.4-r1"
      onBuildRuleCatches "DL3019" "RUN apk add flex=2.6.4-r1"
    it "apk add without --no-cache" $ do
      ruleCatchesNot "DL3019" "RUN apk add --no-cache flex=2.6.4-r1"
      onBuildRuleCatchesNot "DL3019" "RUN apk add --no-cache flex=2.6.4-r1"
