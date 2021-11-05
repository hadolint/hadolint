module Hadolint.Rule.DL3050Spec (spec) where

import Data.Default
import Hadolint (Configuration (..))
import qualified Data.Map as Map
import qualified Hadolint.Rule as Rule
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config =
        def
          { labelSchema = Map.fromList [("required", Rule.RawText)],
            strictLabels = True
          }

  describe "DL3050 - Superfluous label(s) present." $ do
    it "ok with no label" $ do
      ruleCatchesNot "DL3050" ""
      onBuildRuleCatchesNot "DL3050" ""
    it "ok with required label" $ do
      ruleCatchesNot "DL3050" "LABEL required=\"foo\""
      onBuildRuleCatchesNot "DL3050" "LABEL required=\"bar\""
    it "not ok with just other label" $ do
      ruleCatches "DL3050" "LABEL other=\"bar\""
      onBuildRuleCatches "DL3050" "LABEL other=\"bar\""
    it "not ok with other label and required label" $ do
      ruleCatches "DL3050" "LABEL required=\"foo\" other=\"bar\""
      onBuildRuleCatches "DL3050" "LABEL required=\"foo\" other=\"bar\""
