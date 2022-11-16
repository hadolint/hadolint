module Hadolint.Rule.DL3022Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

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
    it "don't warn on external images" $ ruleCatchesNot "DL3022" "COPY --from=haskell:latest bar ."
    it "warn on invalid stage count as --from=<count>" $ ruleCatches "DL3022" "COPY --from=0 bar ."
    it "don't warn on valid stage count as --from=<count>" $
      let dockerFile =
            [ "FROM scratch as build",
              "RUN foo",
              "FROM node",
              "COPY --from=0 foo ."
            ]
      in ruleCatchesNot "DL3022" $ Text.unlines dockerFile
    it "don't warn on valid stage count as --from=<count>" $
      let dockerFile =
            [ "FROM scratch",
              "RUN foo",
              "FROM node",
              "COPY --from=0 foo ."
            ]
      in ruleCatchesNot "DL3022" $ Text.unlines dockerFile