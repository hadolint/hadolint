module Hadolint.Rule.DL3000Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3000 - Use absolute WORKDIR." $ do
    it "workdir relative" $ ruleCatches "DL3000" "WORKDIR relative/dir"
    it "workdir absolute" $ ruleCatchesNot "DL3000" "WORKDIR /usr/local"
    it "workdir variable" $ ruleCatchesNot "DL3000" "WORKDIR ${work}"
    it "workdir relative single quotes" $ ruleCatches "DL3000" "WORKDIR \'relative/dir\'"
    it "workdir absolute single quotes" $ ruleCatchesNot "DL3000" "WORKDIR \'/usr/local\'"
    -- no test for variable/single quotes since the variable would not expand.
    it "workdir relative double quotes" $ ruleCatches "DL3000" "WORKDIR \"relative/dir\""
    it "workdir absolute double quotes" $ ruleCatchesNot "DL3000" "WORKDIR \"/usr/local\""
    it "workdir variable double quotes" $ ruleCatchesNot "DL3000" "WORKDIR \"${dir}\""
    --
    it "workdir absolute windows" $ ruleCatchesNot "DL3000" "WORKDIR \'C:\\\'"
    it "workdir absolute windows quotes" $ ruleCatchesNot "DL3000" "WORKDIR \"C:\\\""
    it "workdir absolute windows alternative" $
      ruleCatchesNot "DL3000" "WORKDIR C:/"
    it "workdir absolute windows quotes alternative" $
      ruleCatchesNot "DL3000" "WORKDIR \"C:/\""
