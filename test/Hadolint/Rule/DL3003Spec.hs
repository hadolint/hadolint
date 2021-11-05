module Hadolint.Rule.DL3003Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3003 - Use WORKDIR to switch to a directory." $ do
    it "ok using WORKDIR" $ ruleCatchesNot "DL3003" "WORKDIR /opt"
    it "not ok using cd" $ ruleCatches "DL3003" "RUN cd /opt"
