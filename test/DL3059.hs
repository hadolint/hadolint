module DL3059 (tests) where

import Data.Text as Text
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
    it "not ok with two `RUN`s separated by a comment" $ do
      ruleCatches "DL3059" "RUN /foo.sh\n# a comment\nRUN /bar.sh"
    it "not ok with two `RUN`s separated by two comment" $ do
      ruleCatches "DL3059" "RUN /foo.sh\n# a comment\n# another comment\nRUN /bar.sh"
    it "ok with one `RUN` after a comment" $ do
      ruleCatchesNot "DL3059" "# a comment\nRUN /foo.sh"
    it "ok with two consecutive `RUN`s when flags are different 1" $ do
      ruleCatchesNot "DL3059" "RUN --mount=type=secret,id=foo /foo.sh\nRUN /bar.sh"
    it "ok with two consecutive `RUN`s when flags are different 2" $ do
      let dfile = [ "RUN --mount=type=secret,id=foo /foo.sh",
                    "RUN --mount=type=secret,id=bar /bar.sh"
                  ]
       in ruleCatchesNot "DL3059" $ Text.unlines dfile
    it "not ok with two consecutive `RUN`s when flags are equal" $ do
      let dfile = [ "RUN --mount=type=secret,id=foo /foo.sh",
                    "RUN --mount=type=secret,id=foo /bar.sh"
                  ]
       in ruleCatches "DL3059" $ Text.unlines dfile
