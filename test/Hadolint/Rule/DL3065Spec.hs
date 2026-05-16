module Hadolint.Rule.DL3065Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def
  let rule = "DL3065"

  describe "DL3065 - Setting `FROM --platform` to predefined `$TARGETPLATFORM` in is redundant as this is the default behavior" $ do

    it "ok: FROM no --platform" $ do
      ruleCatchesNot rule "FROM alpine:3.24"

    it "ok: FROM --platform not $TARGETPLATFORM" $ do
      ruleCatchesNot rule "FROM --platform=$FOOBAR alpine:3.24"
      ruleCatchesNot rule "FROM --platform=foobar alpine:3.24"
      ruleCatchesNot rule "FROM --platform=foobar/arm64 alpine:3.24"

    it "not ok: FROM --platform=$TARGETPLATFORM" $ do
      ruleCatches rule "FROM --platform=$TARGETPLATFORM alpine:3.24"
      ruleCatches rule "FROM --platform=${TARGETPLATFORM} alpine:3.24"
