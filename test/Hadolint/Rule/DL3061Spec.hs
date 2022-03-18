module Hadolint.Rule.DL3061Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3061 - Invalid instruction order. Dockerfile must begin with\
           \ `FROM`, `ARG` or comment." $ do
    it "warn: label before from" $ do
      ruleCatches "DL3061" "LABEL foo=bar\nFROM foo"
      onBuildRuleCatches "DL3061" "LABEL foo=bar\nFROM foo"

    it "don't warn: from before label" $ do
      ruleCatchesNot "DL3061" "FROM foo\nLABEL foo=bar"
      onBuildRuleCatchesNot "DL3061" "FROM foo\nLABEL foo=bar"

    it "don't warn: ARG then FROM then LABEL" $ do
      ruleCatchesNot "DL3061" "ARG A=B\nFROM foo\nLABEL foo=bar"
      onBuildRuleCatchesNot "DL3061" "ARG A=B\nFROM foo\nLABEL foo=bar"

    it "don't warn: pragma before FROM" $ do
      ruleCatchesNot "DL3061" "# syntax = docker/dockerfile:1.0-experimental\n\
                              \FROM node:16-alpine3.13"
      onBuildRuleCatchesNot "DL3061" "# syntax = docker/dockerfile:1.0-experimental\n\
                                     \FROM node:16-alpine3.13"

    it "don't warn: FROM then ARG then RUN" $ do
      ruleCatchesNot "DL3061" "FROM foo\nARG A=B\nRUN echo bla"
