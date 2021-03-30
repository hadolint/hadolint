module DL3002 (tests) where

import Data.Text as Text
import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3002 - Last user should not be root." $ do
    it "last user should not be root" $
      let dockerFile =
            [ "FROM scratch",
              "USER root"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "no root" $
      let dockerFile =
            [ "FROM scratch",
              "USER foo"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "no root UID" $
      let dockerFile =
            [ "FROM scratch",
              "USER 0"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "no root:root" $
      let dockerFile =
            [ "FROM scratch",
              "USER root:root"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "no UID:GID" $
      let dockerFile =
            [ "FROM scratch",
              "USER 0:0"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "can switch back to non root" $
      let dockerFile =
            [ "FROM scratch",
              "USER root",
              "RUN something",
              "USER foo"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "warns on transitive root user" $
      let dockerFile =
            [ "FROM debian as base",
              "USER root",
              "RUN something",
              "FROM base",
              "RUN something else"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "warns on multiple stages" $
      let dockerFile =
            [ "FROM debian as base",
              "USER root",
              "RUN something",
              "FROM scratch",
              "USER foo",
              "RUN something else"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "does not warn when switching in multiple stages" $
      let dockerFile =
            [ "FROM debian as base",
              "USER root",
              "RUN something",
              "USER foo",
              "FROM scratch",
              "RUN something else"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile
