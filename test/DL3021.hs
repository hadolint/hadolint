module DL3021 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3021 - `COPY` with more than 2 arguments requires the last argument to end with `/`" $ do
    it "no warn on 2 args" $ ruleCatchesNot "DL3021" "COPY foo bar"
    it "warn on 3 args" $ ruleCatches "DL3021" "COPY foo bar baz"
    it "no warn on 3 args" $ ruleCatchesNot "DL3021" "COPY foo bar baz/"
