module DL3020 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3020 - Use `COPY` instead of `ADD` for files and folders." $ do
    it "add for tar" $ ruleCatchesNot "DL3020" "ADD file.tar /usr/src/app/"
    it "add for zip" $ ruleCatchesNot "DL3020" "ADD file.zip /usr/src/app/"
    it "add for gzip" $ ruleCatchesNot "DL3020" "ADD file.gz /usr/src/app/"
    it "add for bz2" $ ruleCatchesNot "DL3020" "ADD file.bz2 /usr/src/app/"
    it "add for xz" $ ruleCatchesNot "DL3020" "ADD file.xz /usr/src/app/"
    it "add for tgz" $ ruleCatchesNot "DL3020" "ADD file.tgz /usr/src/app/"
    it "add for url" $ ruleCatchesNot "DL3020" "ADD http://file.com /usr/src/app/"
    it "using add" $ ruleCatches "DL3020" "ADD file /usr/src/app/"
