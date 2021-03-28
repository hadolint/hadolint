module DL3017 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3017 - Do not use `apk upgrade`." $ do
    it "apk upgrade" $ do
      ruleCatches "DL3017" "RUN apk update && apk upgrade"
      onBuildRuleCatches "DL3017" "RUN apk update && apk upgrade"
