module DL3053 (tests) where

import qualified Data.Map as Map
import qualified Hadolint.Process
import qualified Hadolint.Rule as Rule
import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = Hadolint.Process.RulesConfig [] (Map.fromList [("datelabel", Rule.Rfc3339)]) False
  describe "DL3053 - Label `<label>` is not a valid time format - must be conform to RFC3339." $ do
    it "not ok with label not containing RFC3339 date" $ do
      ruleCatches "DL3053" "LABEL datelabel=\"not-date\""
      onBuildRuleCatches "DL3053" "LABEL datelabel=\"not-date\""
    it "ok with label containing RFC3339 date" $ do
      ruleCatchesNot "DL3053" "LABEL datelabel=\"2021-03-10T10:26:33.564595127+01:00\""
      onBuildRuleCatchesNot "DL3053" "LABEL datelabel=\"2021-03-10T10:26:33.564595127+01:00\""
    it "ok with other label not containing RFC3339 date" $ do
      ruleCatchesNot "DL3053" "LABEL other=\"doo\""
      onBuildRuleCatchesNot "DL3053" "LABEL other=\"bar\""
