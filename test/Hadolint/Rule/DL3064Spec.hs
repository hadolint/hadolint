module Hadolint.Rule.DL3064Spec (spec) where

import Data.Default
import qualified Data.Text as Text
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
