module Hadolint.Rule.DL3063Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3063 - Stage name is a reserved word" $ do
    it "not ok: stage name `scratch`" $
      ruleCatches "DL3063" "FROM foo:bar AS scratch"
    it "not ok: stage name `context`" $
      ruleCatches "DL3063" "FROM foo:bar AS context"
    it "ok: stage name `foobar`" $
      ruleCatchesNot "DL3063" "FROM foo:bar AS foobar"
