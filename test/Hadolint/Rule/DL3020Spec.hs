module Hadolint.Rule.DL3020Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3020 - Use `COPY` instead of `ADD` for files and folders." $ do
    it "add for tar" $ ruleCatchesNot "DL3020" "ADD file.tar /usr/src/app/"
    it "add for gzip" $ ruleCatchesNot "DL3020" "ADD file.gz /usr/src/app/"
    it "add for bz2" $ ruleCatchesNot "DL3020" "ADD file.bz2 /usr/src/app/"
    it "add for xz" $ ruleCatchesNot "DL3020" "ADD file.xz /usr/src/app/"
    it "add for tgz" $ ruleCatchesNot "DL3020" "ADD file.tgz /usr/src/app/"
    it "add for url" $ ruleCatchesNot "DL3020" "ADD http://file.com /usr/src/app/"
    it "using add" $ ruleCatches "DL3020" "ADD file /usr/src/app/"
    it "warn for zip" $ ruleCatches "DL3020" "ADD file.zip /usr/src/app/"
    it "add for tgz with quotes" $ ruleCatchesNot "DL3020" "ADD \"file.tgz\" /usr/src/app/"
    it "add for url with quotes" $ ruleCatchesNot "DL3020" "ADD \"http://file.com\" /usr/src/app/"
    it "warn for zip with quotes" $ ruleCatches "DL3020" "ADD \"file.zip\" /usr/src/app/"
