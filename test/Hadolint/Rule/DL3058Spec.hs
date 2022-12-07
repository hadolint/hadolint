module Hadolint.Rule.DL3058Spec (spec) where

import Data.Default
import Hadolint (Configuration (..))
import qualified Data.Map as Map
import qualified Hadolint.Rule as Rule
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def { labelSchema = Map.fromList [("maintainer", Rule.Email)] }

  describe "DL3058 - Label `<label>` is not a valid email format - must conform to RFC5322." $ do
    it "not ok with label not containing valid email" $ do
      ruleCatches "DL3058" "LABEL maintainer=\"not-email\""
      onBuildRuleCatches "DL3058" "LABEL maintainer=\"not-email\""
    it "ok with label containing valid email" $ do
      ruleCatchesNot "DL3058" "LABEL maintainer=\"abcd@google.com\""
      onBuildRuleCatchesNot "DL3058" "LABEL maintainer=\"abcd@google.com\""
    it "ok with other label not containing valid email" $ do
      ruleCatchesNot "DL3058" "LABEL other=\"doo\""
      onBuildRuleCatchesNot "DL3058" "LABEL other=\"bar\""
