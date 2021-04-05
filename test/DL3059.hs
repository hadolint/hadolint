module DL3059 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3059 - Multiple consecutive `RUN` instructions" $ do
    it "ok with no `RUN` at all" $ do
      ruleCatchesNot "DL3059" "FROM debian:10"
    it "ok with one `RUN`" $ do
      ruleCatchesNot "DL3059" "RUN /foo.sh"
    it "ok with two not consecutive `RUN`" $ do
      ruleCatchesNot "DL3059" "RUN /foo.sh\nWORKDIR /\nRUN /bar.sh"
    it "not ok with two consecutive `RUN`s" $ do
      ruleCatches "DL3059" "RUN /foo.sh\nRUN /bar.sh"
