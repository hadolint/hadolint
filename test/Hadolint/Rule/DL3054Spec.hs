module Hadolint.Rule.DL3054Spec (spec) where

import Data.Default
import Hadolint (Configuration (..))
import qualified Data.Map as Map
import qualified Hadolint.Rule as Rule
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def { labelSchema = Map.fromList [("spdxlabel", Rule.Spdx)] }

  describe "DL3054 - Label `<label>` is not a valid SPDX license identifier." $ do
    it "not ok with label not containing SPDX identifier" $ do
      ruleCatches "DL3054" "LABEL spdxlabel=\"not-spdx\""
      onBuildRuleCatches "DL3054" "LABEL spdxlabel=\"not-spdx\""
    it "ok with label containing SPDX identifier" $ do
      ruleCatchesNot "DL3054" "LABEL spdxlabel=\"BSD-3-Clause\""
      onBuildRuleCatchesNot "DL3054" "LABEL spdxlabel=\"MIT\""
    it "ok with other label not containing SPDX identifier" $ do
      ruleCatchesNot "DL3054" "LABEL other=\"fooo\""
      onBuildRuleCatchesNot "DL3054" "LABEL other=\"bar\""
