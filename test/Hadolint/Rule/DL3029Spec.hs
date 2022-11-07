module Hadolint.Rule.DL3029Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3029 - Do not use --platform flag with FROM." $ do
    it "explicit platform flag" $ ruleCatches "DL3029" "FROM --platform=linux debian:jessie"
    it "no platform flag" $ ruleCatchesNot "DL3029" "FROM debian:jessie"
    it "allows platform $BUILDPLATFORM flag" $ ruleCatchesNot "DL3029" "FROM --platform=$BUILDPLATFORM debian:jessie"
    it "allows platform \"$BUILDPLATFORM\" flag" $ ruleCatchesNot "DL3029" "FROM --platform=\"$BUILDPLATFORM\" debian:jessie"
    it "allows platform ${BUILDPLATFORM} flag" $ ruleCatchesNot "DL3029" "FROM --platform=${BUILDPLATFORM} debian:jessie"
    it "allows platform ${BUILDPLATFORM:-} flag" $ ruleCatchesNot "DL3029" "FROM --platform=${BUILDPLATFORM:-} debian:jessie"
    it "allows platform \"${BUILDPLATFORM:-}\" flag" $ ruleCatchesNot "DL3029" "FROM --platform=\"${BUILDPLATFORM:-}\" debian:jessie"
    it "allows platform $TARGETPLATFORM flag" $ ruleCatchesNot "DL3029" "FROM --platform=$TARGETPLATFORM debian:jessie"
