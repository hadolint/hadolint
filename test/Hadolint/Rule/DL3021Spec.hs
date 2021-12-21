module Hadolint.Rule.DL3021Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3021 - `COPY` with more than 2 arguments requires the last argument to end with `/`" $ do
    it "no warn on 2 args" $ ruleCatchesNot "DL3021" "COPY foo bar"
    it "warn on 3 args" $ ruleCatches "DL3021" "COPY foo bar baz"
    it "no warn on 3 args" $ ruleCatchesNot "DL3021" "COPY foo bar baz/"
    it "warn on 3 args with quotes" $ ruleCatches "DL3021" "COPY foo bar \"baz\""
    it "no warn on 3 args with quotes" $ ruleCatchesNot "DL3021" "COPY foo bar \"baz/\""
