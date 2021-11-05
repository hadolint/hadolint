module Hadolint.Rule.DL4000Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def
  describe "DL4000 - MAINTAINER is deprecated." $ do
    it "has deprecated maintainer" $
      ruleCatches "DL4000" "FROM busybox\nMAINTAINER hudu@mail.com"
    it "has maintainer" $ ruleCatches "DL4000" "FROM debian\nMAINTAINER Lukas"
    it "has maintainer first" $ ruleCatches "DL4000" "MAINTAINER Lukas\nFROM DEBIAN"
    it "has no maintainer" $ ruleCatchesNot "DL4000" "FROM debian"
