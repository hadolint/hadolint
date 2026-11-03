module Hadolint.Rule.DL3064Spec (spec) where

import Data.Default
import Data.Text qualified as Text
import Helpers
import Test.Hspec

spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3064 - Potential sensitive data should not be used in `ARG` or `ENV`" $ do
    it "ok: no ARG, no ENV" $ do
      let dockerfile =
            Text.unlines
              [ "FROM debian:bullseye",
                "RUN foobar"
              ]
       in do
            ruleCatchesNot "DL3064" dockerfile
            onBuildRuleCatchesNot "DL3064" dockerfile
    it "ok: ARG no sensitive data, no ENV" $ do
      let dockerfile =
            Text.unlines
              [ "ARG foobar",
                "FROM debian:bullseye",
                "RUN foobar"
              ]
       in do
            ruleCatchesNot "DL3064" dockerfile
            onBuildRuleCatchesNot "DL3064" dockerfile
    it "ok: no ARG, ENV no sensitive data" $ do
      let dockerfile =
            Text.unlines
              [ "FROM debian:bullseye",
                "ENV foobar=barfoo",
                "RUN foobar"
              ]
       in do
            ruleCatchesNot "DL3064" dockerfile
            onBuildRuleCatchesNot "DL3064" dockerfile

    it "not ok: ARG with sensitive data" $ do
      let dockerfile =
            Text.unlines
              [ "ARG AWS_ACCESS_KEY_ID",
                "FROM debian:bullseye",
                "RUN foobar"
              ]
       in do
            ruleCatches "DL3064" dockerfile
            onBuildRuleCatches "DL3064" dockerfile

    it "not ok: ARG with sensitive data, different casing" $ do
      let dockerfile =
            Text.unlines
              [ "ARG openai_api_key",
                "FROM debian:bullseye",
                "RUN foobar"
              ]
       in do
            ruleCatches "DL3064" dockerfile
            onBuildRuleCatches "DL3064" dockerfile

    it "not ok: ENV with sensitive data" $ do
      let dockerfile =
            Text.unlines
              [ "FROM debian:bullseye",
                "ENV AWS_ACCESS_KEY_ID=abcdefghijklmnopqrstuvwxyz",
                "RUN foobar"
              ]
       in do
            ruleCatches "DL3064" dockerfile
            onBuildRuleCatches "DL3064" dockerfile

    it "not ok: ENV with sensitive data, different casing" $ do
      let dockerfile =
            Text.unlines
              [ "FROM debian:bullseye",
                "ENV my_password=abcdefghijklmnopqrstuvwxyz",
                "RUN foobar"
              ]
       in do
            ruleCatches "DL3064" dockerfile
            onBuildRuleCatches "DL3064" dockerfile

    it "ok: ENV with names that only resemble sensitive data" $ do
      let dockerfile =
            Text.unlines
              [ "FROM golang:1.24",
                "ENV GOPRIVATE=github.com/example/*",
                "ENV TRAVIS_OS_NAME=linux TRAVIS_SUDO=false",
                "ENV FACEBOOK_APP_ID=1234567890",
                "RUN foobar"
              ]
       in do
            ruleCatchesNot "DL3064" dockerfile
            onBuildRuleCatchesNot "DL3064" dockerfile

    it "ok: ARG and ENV referencing a secret file" $ do
      let dockerfile =
            Text.unlines
              [ "FROM postgres:17",
                "ARG POSTGRES_PASSWORD_FILE=/run/secrets/db_password",
                "ENV AWS_WEB_IDENTITY_TOKEN_FILE=/var/run/secrets/token",
                "RUN foobar"
              ]
       in do
            ruleCatchesNot "DL3064" dockerfile
            onBuildRuleCatchesNot "DL3064" dockerfile

    it "not ok: ARG with a private key" $ do
      let dockerfile =
            Text.unlines
              [ "ARG SSH_PRIVATE_KEY",
                "FROM debian:bullseye",
                "RUN foobar"
              ]
       in do
            ruleCatches "DL3064" dockerfile
            onBuildRuleCatches "DL3064" dockerfile

    it "not ok: ENV with a sensitive name among other pairs" $ do
      let dockerfile =
            Text.unlines
              [ "FROM debian:bullseye",
                "ENV GOPRIVATE=github.com/example/* GITHUB_TOKEN=abcdef",
                "RUN foobar"
              ]
       in do
            ruleCatches "DL3064" dockerfile
            onBuildRuleCatches "DL3064" dockerfile
