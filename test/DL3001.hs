module DL3001 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3001 - invalid CMD rules" $ do
    it "invalid cmd" $ do
      ruleCatches "DL3001" "RUN top"
      onBuildRuleCatches "DL3001" "RUN top"
    it "install ssh" $ do
      ruleCatchesNot "DL3001" "RUN apt-get install ssh"
      onBuildRuleCatchesNot "DL3001" "RUN apt-get install ssh"
