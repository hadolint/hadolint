module DL3050 (tests) where

import qualified Data.Map as Map
import qualified Hadolint.Process
import qualified Hadolint.Rule as Rule
import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = Hadolint.Process.RulesConfig [] (Map.fromList [("required", Rule.RawText)]) True
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
