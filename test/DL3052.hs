module DL3052 (tests) where

import qualified Data.Map as Map
import qualified Hadolint.Process
import qualified Hadolint.Rule as Rule
import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = Hadolint.Process.RulesConfig [] (Map.fromList [("urllabel", Rule.Url)]) False
  describe "DL3052 - Label `<label>` is not a valid URL." $ do
    it "not ok with label not containing URL" $ do
      ruleCatches "DL3052" "LABEL urllabel=\"not-url\""
      onBuildRuleCatches "DL3052" "LABEL urllabel=\"not-url\""
    it "ok with label containing URL" $ do
      ruleCatchesNot "DL3052" "LABEL urllabel=\"http://example.com\""
      onBuildRuleCatchesNot "DL3052" "LABEL urllabel=\"http://example.com\""
    it "ok with other label not containing URL" $ do
      ruleCatchesNot "DL3052" "LABEL other=\"foo\""
      onBuildRuleCatchesNot "DL3052" "LABEL other=\"bar\""
