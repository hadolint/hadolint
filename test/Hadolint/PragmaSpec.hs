module Hadolint.PragmaSpec (spec) where

import Data.Default
import Helpers
import Test.Hspec
import qualified Data.Text as Text


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "Rules can be ignored with inline comments" $ do

    it "ignores single rule" $
      let dockerFile =
            [ "FROM ubuntu",
              "# hadolint ignore=DL3002",
              "USER root"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "ignores only the given rule" $
      let dockerFile =
            [ "FROM scratch",
              "# hadolint ignore=DL3001",
              "USER root"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "ignores only the given rule, when multiple passed" $
      let dockerFile =
            [ "FROM scratch",
              "# hadolint ignore=DL3001,DL3002",
              "USER root"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "ignores the rule only if directly above the instruction" $
      let dockerFile =
            [ "# hadolint ignore=DL3001,DL3002",
              "FROM ubuntu",
              "USER root"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "won't ignore the rule if passed invalid rule names" $
      let dockerFile =
            [ "FROM scratch",
              "# hadolint ignore=crazy,DL3002",
              "USER root"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "ignores multiple rules correctly, even with some extra whitespace" $
      let dockerFile =
            [ "FROM node as foo",
              "# hadolint ignore=DL3023, DL3021",
              "COPY --from=foo bar baz ."
            ]
       in do
            ruleCatchesNot "DL3023" $ Text.unlines dockerFile
            ruleCatchesNot "DL3021" $ Text.unlines dockerFile


  describe "Rules can be ignored globally with global ignore pragma" $ do

    it "ignores single rule" $
      let dockerFile =
            [ "# hadolint global ignore=DL3002",
              "FROM ubuntu",
              "USER root"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "ignores multiple rules correctly, even with some extra whitespace" $
      let dockerFile =
            [ "# hadolint global ignore = DL3023 , DL3021",
              "FROM node as foo",
              "COPY --from=foo bar baz ."
            ]
       in do
            ruleCatchesNot "DL3023" $ Text.unlines dockerFile
            ruleCatchesNot "DL3021" $ Text.unlines dockerFile
