module DL3027 (tests) where

import Data.Text as Text
import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3027 - Do not use `apt` as it is meant to be a end-user tool, use `apt-get` or `apt-cache` instead" $ do
    it "apt" $
      let dockerFile =
            [ "FROM ubuntu",
              "RUN apt install python"
            ]
       in do
            ruleCatches "DL3027" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3027" $ Text.unlines dockerFile
