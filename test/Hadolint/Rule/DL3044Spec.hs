module Hadolint.Rule.DL3044Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3044 - Do not refer to an environment variable within the same `ENV` statement where it is defined." $ do
    it "ok with normal ENV" $
      ruleCatchesNot "DL3044" "ENV BLA=\"blubb\"\nENV BLUBB=\"${BLA}/blubb\""
    it "ok with partial match 1" $
      ruleCatchesNot "DL3044" "ENV BLA=\"blubb\" BLUBB=\"${FOOBLA}/blubb\""
    it "ok with partial match 2" $
      ruleCatchesNot "DL3044" "ENV BLA=\"blubb\" BLUBB=\"${BLAFOO}/blubb\""
    it "ok with partial match 3" $
      ruleCatchesNot "DL3044" "ENV BLA=\"blubb\" BLUBB=\"$FOOBLA/blubb\""
    it "ok with partial match 4" $
      ruleCatchesNot "DL3044" "ENV BLA=\"blubb\" BLUBB=\"$BLAFOO/blubb\""
    it "fail with partial match 5" $
      ruleCatches "DL3044" "ENV BLA=\"blubb\" BLUBB=\"$BLA/$BLAFOO/blubb\""
    it "ok with parial match 6" $
      ruleCatchesNot "DL3044" "ENV BLA=\"blubb\" BLUBB=\"BLA/$BLAFOO/BLA\""
    it "ok when previously defined in `ARG`" $
      ruleCatchesNot "DL3044" "ARG BLA\nENV BLA=${BLA}"
    it "ok when previously defined in `ENV`" $
      ruleCatchesNot "DL3044" "ENV BLA blubb\nENV BLA=${BLA}"
    it "ok with referencing a variable on its own right hand side" $
      ruleCatchesNot "DL3044" "ENV PATH=/bla:${PATH}"
    it "ok with referencing a variable on its own right side twice in different `ENV`s" $
      ruleCatchesNot "DL3044" "ENV PATH=/bla:${PATH}\nENV PATH=/blubb:${PATH}"
    it "fail when referencing a variable on its own right side twice within the same `ENV`" $
      ruleCatches "DL3044" "ENV PATH=/bla:${PATH} PATH=/blubb:${PATH}"
    it "fail with selfreferencing with curly braces ENV" $
      ruleCatches "DL3044" "ENV BLA=\"blubb\" BLUBB=\"${BLA}/blubb\""
    it "fail with selfreferencing without curly braces ENV" $
      ruleCatches "DL3044" "ENV BLA=\"blubb\" BLUBB=\"$BLA/blubb\""
    it "fail with full match 1" $
      ruleCatches "DL3044" "ENV BLA=\"blubb\" BLUBB=\"$BLA\""
    it "fail with full match 2" $
      ruleCatches "DL3044" "ENV BLA=\"blubb\" BLUBB=\"${BLA}\""
