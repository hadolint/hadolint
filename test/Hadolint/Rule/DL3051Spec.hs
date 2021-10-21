module Hadolint.Rule.DL3051Spec (spec) where

import Data.Default
import Hadolint (Configuration (..))
import qualified Data.Map as Map
import qualified Hadolint.Rule as Rule
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config =
        def { labelSchema = Map.fromList [("emptylabel", Rule.RawText)] }
  describe "DL3051 - Label `<label>` is empty." $ do
    it "not ok with label empty" $ do
      ruleCatches "DL3051" "LABEL emptylabel=\"\""
      onBuildRuleCatches "DL3051" "LABEL emptylabel=\"\""
    it "ok with label not empty" $ do
      ruleCatchesNot "DL3051" "LABEL emptylabel=\"foo\""
      onBuildRuleCatchesNot "DL3051" "LABEL emptylabel=\"bar\""
    it "ok with other label empty" $ do
      ruleCatchesNot "DL3051" "LABEL other=\"\""
      onBuildRuleCatchesNot "DL3051" "LABEL other=\"\""
