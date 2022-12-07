module Hadolint.Rule.DL3053Spec (spec) where

import Data.Default
import Hadolint (Configuration (..))
import qualified Data.Map as Map
import qualified Hadolint.Rule as Rule
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def { labelSchema = Map.fromList [("datelabel", Rule.Rfc3339)] }

  describe "DL3053 - Label `<label>` is not a valid time format - must conform to RFC3339." $ do
    it "not ok with label not containing RFC3339 date" $ do
      ruleCatches "DL3053" "LABEL datelabel=\"not-date\""
      onBuildRuleCatches "DL3053" "LABEL datelabel=\"not-date\""
    it "ok with label containing RFC3339 date" $ do
      ruleCatchesNot "DL3053" "LABEL datelabel=\"2021-03-10T10:26:33.564595127+01:00\""
      onBuildRuleCatchesNot "DL3053" "LABEL datelabel=\"2021-03-10T10:26:33.564595127+01:00\""
    it "ok with other label not containing RFC3339 date" $ do
      ruleCatchesNot "DL3053" "LABEL other=\"doo\""
      onBuildRuleCatchesNot "DL3053" "LABEL other=\"bar\""
