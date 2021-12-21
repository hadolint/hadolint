module Hadolint.Rule.DL1001Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL1001 - Please don't use inline ignore pragma." $ do
    it "catches inline ignore pragma" $ do
      let file =
            [ "# hadolint ignore=DL3003",
              "RUN foo bar"
            ]
       in do
        ruleCatches "DL1001" $ Text.unlines file
    it "does not catch other pragma" $ do
      let file =
            [ "# hadolint shell=powershell",
              "RUN foo bar"
            ]
       in do
        ruleCatchesNot "DL1001" $ Text.unlines file
    it "does not catch other comment" $ do
      let file =
            [ "# foobar",
              "RUN foo bar"
            ]
       in do
        ruleCatchesNot "DL1001" $ Text.unlines file
    it "does not catch when inline ignore pragma is absent" $ do
      let file =
            [ "FROM debian",
              "RUN foo bar"
            ]
       in do
        ruleCatchesNot "DL1001" $ Text.unlines file
