module DL3046 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3046 - `useradd` without flag `-l` and high UID will result in excessively large Image." $ do
    it "ok with `useradd` alone" $ ruleCatchesNot "DL3046" "RUN useradd luser"
    it "ok with `useradd` short uid" $ ruleCatchesNot "DL3046" "RUN useradd -u 12345 luser"
    it "ok with `useradd` long uid and flag `-l`" $ ruleCatchesNot "DL3046" "RUN useradd -l -u 123456 luser"
    it "ok with `useradd` and just flag `-l`" $ ruleCatchesNot "DL3046" "RUN useradd -l luser"
    it "warn when `useradd` and long uid without flag `-l`" $ ruleCatches "DL3046" "RUN useradd -u 123456 luser"
