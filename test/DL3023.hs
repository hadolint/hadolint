module DL3023 (tests) where

import Data.Text as Text
import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3023 - `COPY --from` cannot reference its own `FROM` alias" $ do
    it "warn on copying from your the same FROM" $
      let dockerFile =
            [ "FROM node as foo",
              "COPY --from=foo bar ."
            ]
       in ruleCatches "DL3023" $ Text.unlines dockerFile
    it "don't warn on copying from other sources" $
      let dockerFile =
            [ "FROM scratch as build",
              "RUN foo",
              "FROM node as run",
              "COPY --from=build foo .",
              "RUN baz"
            ]
       in ruleCatchesNot "DL3023" $ Text.unlines dockerFile
