module Hadolint.Rule.DL3027Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3027 - Do not use `apt` as it is meant to be an end-user tool, use `apt-get` or `apt-cache` instead" $ do
    it "apt" $
      let dockerFile =
            [ "FROM ubuntu",
              "RUN apt install python"
            ]
       in do
            ruleCatches "DL3027" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3027" $ Text.unlines dockerFile
