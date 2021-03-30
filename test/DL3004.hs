module DL3004 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3004 - Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce root." $ do
    it "sudo" $ do
      ruleCatches "DL3004" "RUN sudo apt-get update"
      onBuildRuleCatches "DL3004" "RUN sudo apt-get update"
    it "install sudo" $ do
      ruleCatchesNot "DL3004" "RUN apt-get install sudo"
      onBuildRuleCatchesNot "DL3004" "RUN apt-get install sudo"
    it "sudo chained programs" $ do
      ruleCatches "DL3004" "RUN apt-get update && sudo apt-get install"
      onBuildRuleCatches "DL3004" "RUN apt-get update && sudo apt-get install"
