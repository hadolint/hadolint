module Hadolint.Rule.DL3045Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec

spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3045 - `COPY` without `WORKDIR` set" $ do
    it "ok: `COPY` with absolute destination and no `WORKDIR` set" $ do
      ruleCatchesNot "DL3045" "COPY bla.sh /usr/local/bin/blubb.sh"
      onBuildRuleCatchesNot "DL3045" "COPY bla.sh /usr/local/bin/blubb.sh"

    it "ok: `COPY` with absolute destination and no `WORKDIR` set with quotes" $ do
      ruleCatchesNot "DL3045" "COPY bla.sh \"/usr/local/bin/blubb.s\""
      onBuildRuleCatchesNot "DL3045" "COPY bla.sh \"/usr/local/bin/blubb.sh\""

    it "ok: `COPY` with absolute destination and no `WORKDIR` set - windows" $ do
      ruleCatchesNot "DL3045" "COPY bla.sh c:\\system32\\blubb.sh"
      onBuildRuleCatchesNot "DL3045" "COPY bla.sh d:\\mypath\\blubb.sh"

    it "ok: `COPY` with absolute destination and no `WORKDIR` set - windows with quotes" $ do
      ruleCatchesNot "DL3045" "COPY bla.sh \"c:\\system32\\blubb.sh\""
      onBuildRuleCatchesNot "DL3045" "COPY bla.sh \"d:\\mypath\\blubb.sh\""

    it "ok: `COPY` with absolute destination and no `WORKDIR` set - windows with alternative paths" $ do
      ruleCatchesNot "DL3045" "COPY bla.sh c:/system32/blubb.sh"
      onBuildRuleCatchesNot "DL3045" "COPY bla.sh d:/mypath/blubb.sh"

    it "ok: `COPY` with relative destination and `WORKDIR` set" $ do
      ruleCatchesNot "DL3045" "FROM scratch\nWORKDIR /usr\nCOPY bla.sh blubb.sh"
      onBuildRuleCatchesNot "FROM scratch\nDL3045" "WORKDIR /usr\nCOPY bla.sh blubb.sh"

    it "ok: `COPY` with relative destination and `WORKDIR` set - windows" $ do
      ruleCatchesNot "DL3045" "FROM scratch\nWORKDIR c:\\system32\nCOPY bla.sh blubb.sh"
      onBuildRuleCatchesNot "DL3045" "FROM scratch\nWORKDIR c:\\system32\nCOPY bla.sh blubb.sh"

    it "ok: `COPY` with destination being an environment variable 1" $ do
      ruleCatchesNot "DL3045" "COPY src.sh ${SRC_BASE_ENV}"
      onBuildRuleCatchesNot "DL3045" "COPY src.sh ${SRC_BASE_ENV}"

    it "ok: `COPY` with destination being an environment variable 2" $ do
      ruleCatchesNot "DL3045" "COPY src.sh $SRC_BASE_ENV"
      onBuildRuleCatchesNot "DL3045" "COPY src.sh $SRC_BASE_ENV"

    it "ok: `COPY` with destination being an environment variable 3" $ do
      ruleCatchesNot "DL3045" "COPY src.sh \"${SRC_BASE_ENV}\""
      onBuildRuleCatchesNot "DL3045" "COPY src.sh \"${SRC_BASE_ENV}\""

    it "ok: `COPY` with destination being an environment variable 4" $ do
      ruleCatchesNot "DL3045" "COPY src.sh \"$SRC_BASE_ENV\""
      onBuildRuleCatchesNot "DL3045" "COPY src.sh \"$SRC_BASE_ENV\""

    it "not ok: `COPY` with relative destination and no `WORKDIR` set" $ do
      ruleCatches "DL3045" "COPY bla.sh blubb.sh"
      onBuildRuleCatches "DL3045" "COPY bla.sh blubb.sh"

    it "not ok: `COPY` with relative destination and no `WORKDIR` set with quotes" $ do
      ruleCatches "DL3045" "COPY bla.sh \"blubb.sh\""
      onBuildRuleCatches "DL3045" "COPY bla.sh \"blubb.sh\""

    it "not ok: `COPY` to relative destination if `WORKDIR` is set in a previous stage but not inherited" $
      let dockerFile =
            Text.unlines
              [ "FROM debian:buster as stage1",
                "WORKDIR /usr",
                "FROM debian:buster",
                "COPY foo bar"
              ]
       in do
            ruleCatches "DL3045" dockerFile
            onBuildRuleCatches "DL3045" dockerFile

    it "not ok: `COPY` to relative destination if `WORKDIR` is set in a previous stage but not inherited - windows" $
      let dockerFile =
            Text.unlines
              [ "FROM microsoft/windowsservercore as stage1",
                "WORKDIR c:\\system32",
                "FROM microsoft/windowsservercore",
                "COPY foo bar"
              ]
       in do
            ruleCatches "DL3045" dockerFile
            onBuildRuleCatches "DL3045" dockerFile

    it "ok: `COPY` to relative destination if `WORKDIR` has been set in base image" $
      let dockerFile =
            Text.unlines
              [ "FROM debian:buster as base",
                "WORKDIR /usr",
                "FROM debian:buster as stage-inbetween",
                "RUN foo",
                "FROM base",
                "COPY foo bar"
              ]
       in do
            ruleCatchesNot "DL3045" dockerFile
            onBuildRuleCatchesNot "DL3045" dockerFile

    it "ok: `COPY` to relative destination if `WORKDIR` has been set in base image - windows" $
      let dockerFile =
            Text.unlines
              [ "FROM microsoft/windowsservercore as base",
                "WORKDIR c:\\system32",
                "FROM microsoft/windowsservercore as stage-inbetween",
                "RUN foo",
                "FROM base",
                "COPY foo bar"
              ]
       in do
            ruleCatchesNot "DL3045" dockerFile
            onBuildRuleCatchesNot "DL3045" dockerFile

    it "ok: `COPY` to relative destination if `WORKDIR` has been set in previous stage, deep case" $
      let dockerFile =
            Text.unlines
              [ "FROM debian:buster as base1",
                "WORKDIR /usr",
                "FROM base1 as base2",
                "RUN foo",
                "FROM base2",
                "COPY foo bar"
              ]
       in do
            ruleCatchesNot "DL3045" dockerFile
            onBuildRuleCatchesNot "DL3045" dockerFile

    it "ok: `COPY` to relative destination if `WORKDIR` has been set in previous stage, deep case - windows" $
      let dockerFile =
            Text.unlines
              [ "FROM microsoft/windowsservercore as base1",
                "WORKDIR c:\\system32",
                "FROM base1 as base2",
                "RUN foo",
                "FROM base2",
                "COPY foo bar"
              ]
       in do
            ruleCatchesNot "DL3045" dockerFile
            onBuildRuleCatchesNot "DL3045" dockerFile

    it "ok: `COPY` to relative destination if `WORKDIR` has been set, both within an `ONBUILD` context" $
      let dockerFile =
            Text.unlines
              [ "FROM debian:buster",
                "ONBUILD WORKDIR /usr/local/lib",
                "ONBUILD COPY foo bar"
              ]
       in do
            ruleCatchesNot "DL3045" dockerFile
            onBuildRuleCatchesNot "DL3045" dockerFile

    it "regression: don't crash with single character paths" $ do
      ruleCatches "DL3045" "COPY a b"
      onBuildRuleCatches "DL3045" "COPY a b"
