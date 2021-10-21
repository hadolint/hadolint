module Hadolint.Rule.DL3001Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3001 - invalid CMD rules" $ do
    it "invalid cmd" $ do
      ruleCatches "DL3001" "RUN top"
      onBuildRuleCatches "DL3001" "RUN top"
    it "install ssh" $ do
      ruleCatchesNot "DL3001" "RUN apt-get install ssh"
      onBuildRuleCatchesNot "DL3001" "RUN apt-get install ssh"
