module DL3003 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3003 - Use WORKDIR to switch to a directory." $ do
    it "ok using WORKDIR" $ ruleCatchesNot "DL3003" "WORKDIR /opt"
    it "not ok using cd" $ ruleCatches "DL3003" "RUN cd /opt"
