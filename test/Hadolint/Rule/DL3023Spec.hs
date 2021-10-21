module Hadolint.Rule.DL3023Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

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
