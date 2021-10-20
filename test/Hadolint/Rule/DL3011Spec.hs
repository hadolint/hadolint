module Hadolint.Rule.DL3011Spec (spec) where

import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = mempty
  describe "EXPOSE rules" $ do
    it "invalid port" $ ruleCatches "DL3011" "EXPOSE 80000"
    it "valid port" $ ruleCatchesNot "DL3011" "EXPOSE 60000"
