module DL3060 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3060 - `yarn cache clean` missing after `yarn install`" $ do
    it "ok with non-yarn commands" $ do
      ruleCatchesNot "DL3060" "RUN foo"
      onBuildRuleCatchesNot "DL3060" "RUN foo"
    it "not ok with no cache clean" $ do
      ruleCatches "DL3060" "RUN yarn install foo"
      onBuildRuleCatches "DL3060" "RUN yarn install foo"
    it "ok with cache clean" $ do
      ruleCatchesNot "DL3060" "RUN yarn install bar && yarn cache clean"
      onBuildRuleCatchesNot "DL3060" "RUN yarn install bar && yarn cache clean"
