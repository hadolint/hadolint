module Hadolint.Rule.DL4007Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL4007 - COPY with dot as source" $ do
    it "warns on COPY . ." $
      let dockerFile =
            [ "FROM scratch",
              "COPY . ."
            ]
       in ruleCatches "DL4007" $ Text.unlines dockerFile

    it "warns on COPY . /app" $
      let dockerFile =
            [ "FROM scratch",
              "COPY . /app"
            ]
       in ruleCatches "DL4007" $ Text.unlines dockerFile

    it "warns on COPY './' /app" $
      let dockerFile =
            [ "FROM scratch",
              "COPY './' /app"
            ]
       in ruleCatches "DL4007" $ Text.unlines dockerFile

    it "does not warn on COPY src/ ." $
      let dockerFile =
            [ "FROM scratch",
              "COPY src/ ."
            ]
       in ruleCatchesNot "DL4007" $ Text.unlines dockerFile

    it "does not warn on specific COPY" $
      let dockerFile =
            [ "FROM scratch",
              "COPY package.json yarn.lock /app/"
            ]
       in ruleCatchesNot "DL4007" $ Text.unlines dockerFile

    it "warns on COPY . . even after FROM alias" $
      let dockerFile =
            [ "FROM node:18 as build",
              "COPY . .",
              "RUN echo hello"
            ]
       in ruleCatches "DL4007" $ Text.unlines dockerFile
