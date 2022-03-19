module Hadolint.Rule.DL3047Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3047 - `wget` without flag `--progress` will result in excessively bloated build logs when downloading larger files." $ do
    it "warns when using wget without --progress option" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget my.xyz"
            ]
       in do ruleCatches "DL3047" $ Text.unlines dockerFile
             onBuildRuleCatches "DL3047" $ Text.unlines dockerFile
    it "does not warn when running with --progress option" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget --progress=dot:giga my.xyz"
            ]
       in do ruleCatchesNot "DL3047" $ Text.unlines dockerFile
             onBuildRuleCatchesNot "DL3047" $ Text.unlines dockerFile
    it "does not warn when running with -q (quiet short option) and without --progress option" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget -q my.xyz"
            ]
       in do ruleCatchesNot "DL3047" $ Text.unlines dockerFile
             onBuildRuleCatchesNot "DL3047" $ Text.unlines dockerFile
    it "does not warn when running with --quiet (quiet long option) and without --progress option" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget --quiet my.xyz"
            ]
       in do ruleCatchesNot "DL3047" $ Text.unlines dockerFile
             onBuildRuleCatchesNot "DL3047" $ Text.unlines dockerFile
    it "does not warn when running with -nv (no-verbose short option) and without --progress option" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget -nv my.xyz"
            ]
       in do ruleCatchesNot "DL3047" $ Text.unlines dockerFile
             onBuildRuleCatchesNot "DL3047" $ Text.unlines dockerFile
    it "does not warn when running with --no-verbose (no-verbose long option) and without --progress option" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget --no-verbose my.xyz"
            ]
       in do ruleCatchesNot "DL3047" $ Text.unlines dockerFile
             onBuildRuleCatchesNot "DL3047" $ Text.unlines dockerFile
    it "does not warn when running with --output-file (output-file long option) and without --progress option" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget --output-file=/tmp/wget.log my.xyz"
            ]
       in do ruleCatchesNot "DL3047" $ Text.unlines dockerFile
             onBuildRuleCatchesNot "DL3047" $ Text.unlines dockerFile
    it "does not warn when running with -o (output-file long option) and without --progress option" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget -o /tmp/wget.log my.xyz"
            ]
       in do ruleCatchesNot "DL3047" $ Text.unlines dockerFile
             onBuildRuleCatchesNot "DL3047" $ Text.unlines dockerFile
    it "does not warn when running with --append-output (append-output long option) and without --progress option" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget --append-output=/tmp/wget.log my.xyz"
            ]
       in do ruleCatchesNot "DL3047" $ Text.unlines dockerFile
             onBuildRuleCatchesNot "DL3047" $ Text.unlines dockerFile
    it "does not warn when running with -a (append-output long option) and without --progress option" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget -a /tmp/wget.log my.xyz"
            ]
       in do ruleCatchesNot "DL3047" $ Text.unlines dockerFile
             onBuildRuleCatchesNot "DL3047" $ Text.unlines dockerFile
