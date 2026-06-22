module Hadolint.Rule.DL3066Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def
  let rule = "DL3066"

  describe "DL3066 Non-numeric user-id may not be resolvable by host system" $ do

    it "ok: numeric UID" $ do
      ruleCatchesNot rule "USER 12345"

    it "ok: numeric UID and GID" $ do
      ruleCatchesNot rule "USER 1234:5678"

    it "not ok: non-numeric UID" $ do
      ruleCatches rule "USER foobar"

    it "not ok: non-numeric UID and GID" $ do
      ruleCatches rule "USER foobar:barfoo"
