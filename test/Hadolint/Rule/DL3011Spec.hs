module Hadolint.Rule.DL3011Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3011 - Valid UNIX ports range from 0 to 65535" $ do
    describe "DL3011 - single port" $ do
      it "invalid port" $ ruleCatches "DL3011" "EXPOSE 80000"
      it "valid port" $ ruleCatchesNot "DL3011" "EXPOSE 60000"
      it "valid port variable" $ ruleCatchesNot "DL3011" "EXPOSE ${FOOBAR}"

    describe "DL3011 - port range" $ do
      it "invalid port in range" $ ruleCatches "DL3011" "EXPOSE 40000-80000/tcp"
      it "valid port range" $ ruleCatchesNot "DL3011" "EXPOSE 40000-60000/tcp"
      it "valid port range variable" $ ruleCatchesNot "DL3011" "EXPOSE 40000-${FOOBAR}"
