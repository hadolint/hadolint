module Hadolint.Rule.DL3066Spec (spec) where

import Data.Default
import Data.Text qualified as Text
import Helpers
import Test.Hspec

spec :: SpecWith ()
spec = do
  let ?config = def
  let rule = "DL3066"

  describe "DL3066 Non-numeric user-id may not be resolvable by host system" $ do
    it "ok: numeric UID" $ do
      let dockerfile = Text.unlines ["FROM foo:bar", "USER 1234"]
      ruleCatchesNot rule dockerfile

    it "ok: numeric UID and GID" $ do
      let dockerfile = Text.unlines ["FROM foo:bar", "USER 1234:5678"]
      ruleCatchesNot rule dockerfile

    it "not ok: non-numeric UID" $ do
      let dockerfile = Text.unlines ["FROM foo:bar", "USER foobar"]
      ruleCatches rule dockerfile

    it "not ok: non-numeric UID and GID" $ do
      let dockerfile = Text.unlines ["FROM foo:bar", "USER foobar:barfoo"]
      ruleCatches rule dockerfile

    it "ok: UID is non-numeric, but is a defined ARG" $ do
      let dockerfile = Text.unlines ["ARG APP_UID", "FROM foobar:barfoo", "USER ${APP_UID}"]
       in ruleCatchesNot rule dockerfile

    it "ok: UID is non-numeric in any but last build stage" $ do
      let dockerfile =
            Text.unlines
              [ "FROM foo:bar AS build",
                "USER foobar",
                "FROM foobar:barfoo",
                "USER 1234"
              ]
       in ruleCatchesNot rule dockerfile

    it "not ok: UID is numeric not in last build stage" $ do
      let dockerfile =
            Text.unlines
              [ "FROM foo:bar AS build",
                "USER 1234",
                "FROM foobar:barfoo",
                "USER foobar"
              ]
       in ruleCatches rule dockerfile

    it "ok: UID is non-numeric but USER with numeric UID follows" $ do
      let dockerfile =
            Text.unlines
              [ "FROM foo:bar",
                "USER foobar",
                "USER 1234"
              ]
       in ruleCatchesNot rule dockerfile

    it "not ok: UID is numeric but USER with non-numeric UID follows" $ do
      let dockerfile =
            Text.unlines
              [ "FROM foo:bar",
                "USER 1234",
                "USER foobar"
              ]
       in ruleCatches rule dockerfile
    it "ok: UID and GID are non-numeric, but are defined ENVs" $ do
      ruleCatchesNot rule "ENV APP_UID=1000\nUSER $APP_UID"
      ruleCatchesNot rule "ENV USER_UID=1000 USER_GID=1000\nUSER ${USER_UID}:${USER_GID}"
      onBuildRuleCatchesNot rule "ENV APP_UID=1000\nUSER $APP_UID"
    it "not ok: UID is a variable that isn't defined" $ do
      ruleCatches rule "ENV APP_UID=1000\nUSER $OTHER_UID"
      onBuildRuleCatches rule "ENV APP_UID=1000\nUSER $OTHER_UID"
