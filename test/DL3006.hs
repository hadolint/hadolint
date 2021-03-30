module DL3006 (tests) where

import Data.Text as Text
import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3006 - Always tag the version of an image explicitly." $ do
    it "no untagged" $ ruleCatches "DL3006" "FROM debian"
    it "no untagged with name" $ ruleCatches "DL3006" "FROM debian AS builder"
    it "untagged digest is not an error" $
      ruleCatchesNot "DL3006" "FROM ruby@sha256:f1dbca0f5dbc9"
    it "untagged digest is not an error" $
      ruleCatchesNot "DL3006" "FROM ruby:2"
    it "local aliases are OK to be untagged" $
      let dockerFile =
            [ "FROM golang:1.9.3-alpine3.7 AS build",
              "RUN foo",
              "FROM build as unit-test",
              "RUN bar",
              "FROM alpine:3.7",
              "RUN baz"
            ]
       in do
            ruleCatchesNot "DL3006" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3006" $ Text.unlines dockerFile
    it "other untagged cases are not ok" $
      let dockerFile =
            [ "FROM golang:1.9.3-alpine3.7 AS build",
              "RUN foo",
              "FROM node as unit-test",
              "RUN bar",
              "FROM alpine:3.7",
              "RUN baz"
            ]
       in do
            ruleCatches "DL3006" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3006" $ Text.unlines dockerFile
    it "scratch" $ ruleCatchesNot "DL3006" "FROM scratch"
