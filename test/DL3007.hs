module DL3007 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3007 - Using latest is prone to errors if the image will ever update. Pin the version explicitly to a release tag." $ do
    it "explicit latest" $ ruleCatches "DL3007" "FROM debian:latest"
    it "explicit latest with name" $ ruleCatches "DL3007" "FROM debian:latest AS builder"
    it "explicit tagged" $ ruleCatchesNot "DL3007" "FROM debian:jessie"
    it "explicit SHA" $
      ruleCatchesNot
        "DL3007"
        "FROM hub.docker.io/debian@sha256:\
        \7959ed6f7e35f8b1aaa06d1d8259d4ee25aa85a086d5c125480c333183f9deeb"
    it "explicit tagged with name" $
      ruleCatchesNot "DL3007" "FROM debian:jessie AS builder"
