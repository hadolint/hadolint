module Hadolint.Rule.DL3024Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3024 - Duplicate aliases" $ do
    it "warn on duplicate aliases" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN something",
              "FROM scratch as foo",
              "RUN something"
            ]
       in ruleCatches "DL3024" $ Text.unlines dockerFile
    it "don't warn on unique aliases" $
      let dockerFile =
            [ "FROM scratch as build",
              "RUN foo",
              "FROM node as run",
              "RUN baz"
            ]
       in ruleCatchesNot "DL3024" $ Text.unlines dockerFile
