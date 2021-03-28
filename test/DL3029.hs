module DL3029 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3029 - Do not use --platform flag with FROM." $ do
    it "explicit platform flag" $ ruleCatches "DL3029" "FROM --platform=linux debian:jessie"
    it "no platform flag" $ ruleCatchesNot "DL3029" "FROM debian:jessie"
