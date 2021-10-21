module Hadolint.Rule.DL3052Spec (spec) where

import Data.Default
import Hadolint (Configuration (..), LabelType (..))
import qualified Data.Map as Map
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def { labelSchema = Map.fromList [("urllabel", Url)] }

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
