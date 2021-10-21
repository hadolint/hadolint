module Hadolint.Rule.DL3012Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3012 - Multiple `HEALTHCHECK` instructions" $ do
    it "ok with no HEALTHCHECK instruction" $
      ruleCatchesNot "DL3012" "FROM scratch"
    it "ok with one HEALTHCHECK instruction" $
      ruleCatchesNot "DL3012" "FROM scratch\nHEALTHCHECK CMD /bin/bla"
    it "ok with two HEALTHCHECK instructions in two stages" $
      ruleCatchesNot "DL3012" "FROM scratch\nHEALTHCHECK CMD /bin/bla1\nFROM scratch\nHEALTHCHECK CMD /bin/bla2"
    it "warn with two HEALTHCHECK instructions" $
      ruleCatches "DL3012" "FROM scratch\nHEALTHCHECK CMD /bin/bla1\nHEALTHCHECK CMD /bin/bla2"
