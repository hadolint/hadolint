module Hadolint.Rule.DL3063Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec

spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3063 - A USER instruction should be specified" $ do
    it "no USER instruction" $ do
      ruleCatches "DL3063" "FROM alpine:3"
      onBuildRuleCatches "DL3063" "FROM alpine:3"
    it "USER instruction present" $ do
      ruleCatchesNot "DL3063" "FROM alpine:3\nUSER nobody"
      onBuildRuleCatchesNot "DL3063" "FROM alpine:3\nUSER nobody"
    it "USER instruction in multi-stage build" $ do
      ruleCatchesNot "DL3063" "FROM alpine:3 AS builder\nFROM alpine:3\nUSER app"
      onBuildRuleCatchesNot "DL3063" "FROM alpine:3 AS builder\nFROM alpine:3\nUSER app"
    it "USER with UID" $ do
      ruleCatchesNot "DL3063" "FROM alpine:3\nUSER 1000"
      onBuildRuleCatchesNot "DL3063" "FROM alpine:3\nUSER 1000"
    it "USER with UID:GID" $ do
      ruleCatchesNot "DL3063" "FROM alpine:3\nUSER 1000:1000"
      onBuildRuleCatchesNot "DL3063" "FROM alpine:3\nUSER 1000:1000"
      ruleCatchesNot "DL3063" "FROM alpine:3\nUSER 1000:1000"
      onBuildRuleCatchesNot "DL3063" "FROM alpine:3\nUSER 1000:1000"
