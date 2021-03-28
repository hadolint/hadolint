module DL3011 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "EXPOSE rules" $ do
    it "invalid port" $ ruleCatches "DL3011" "EXPOSE 80000"
    it "valid port" $ ruleCatchesNot "DL3011" "EXPOSE 60000"
