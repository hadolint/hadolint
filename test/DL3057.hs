module DL3057 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3057 - `HEALTHCHECK instruction missing" $ do
    it "warn with no HEALTHCHECK instructions" $
      ruleCatches "DL3057" "FROM scratch"
    it "ok with one HEALTHCHECK instruction" $
      ruleCatchesNot "DL3057" "FROM scratch\nHEALTHCHECK CMD /bin/bla"
    it "ok with inheriting HEALTHCHECK instruction" $
      ruleCatchesNot "DL3057" "FROM scratch AS base\nHEALTHCHECK CMD /bin/bla\nFROM base"
    it "warn when not inheriting with no HEALTHCHECK instruction" $
      ruleCatches "DL3057" "FROM scratch AS base\nHEALTHCHECK CMD /bin/bla\nFROM scratch"
