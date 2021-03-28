module DL3022 (tests) where

import Data.Text as Text
import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3022 - `COPY --from` should reference a previously defined `FROM` alias" $ do
    it "warn on missing alias" $ ruleCatches "DL3022" "COPY --from=foo bar ."
    it "warn on alias defined after" $
      let dockerFile =
            [ "FROM scratch",
              "COPY --from=build foo .",
              "FROM node as build",
              "RUN baz"
            ]
       in ruleCatches "DL3022" $ Text.unlines dockerFile
    it "don't warn on correctly defined aliases" $
      let dockerFile =
            [ "FROM scratch as build",
              "RUN foo",
              "FROM node",
              "COPY --from=build foo .",
              "RUN baz"
            ]
       in ruleCatchesNot "DL3022" $ Text.unlines dockerFile
