module Hadolint.Rule.DL3067Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def
  let rule = "DL3067"

  describe "DL3067 - Do not copy an entire filesystem from another stage" $ do
    it "warns when copying the root filesystem from another stage to root" $
      ruleCatches rule "COPY --from=build / /"

    it "warns when copying quoted root paths from another stage to root" $
      ruleCatches rule "COPY --from=build \"/\" \"/\""

    it "does not warn when copying root without --from" $
      ruleCatchesNot rule "COPY / /"

    it "does not warn when copying a directory from another stage" $
      ruleCatchesNot rule "COPY --from=build /app /"

    it "does not warn when copying root from another stage into a subdirectory" $
      ruleCatchesNot rule "COPY --from=build / /app"
