module DL3042 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3042 - Avoid cache directory with `pip install --no-cache-dir <package>`." $ do
    it "pip2 --no-cache-dir not used" $ do
      ruleCatches "DL3042" "RUN pip2 install MySQL_python"
      onBuildRuleCatches "DL3042" "RUN pip2 install MySQL_python"
    it "pip3 --no-cache-dir not used" $ do
      ruleCatches "DL3042" "RUN pip3 install MySQL_python"
      onBuildRuleCatches "DL3042" "RUN pip3 install MySQL_python"
    it "pip --no-cache-dir not used" $ do
      ruleCatches "DL3042" "RUN pip install MySQL_python"
      onBuildRuleCatches "DL3042" "RUN pip install MySQL_python"
    it "pip2 --no-cache-dir used" $ do
      ruleCatchesNot "DL3042" "RUN pip2 install MySQL_python --no-cache-dir"
      onBuildRuleCatchesNot "DL3042" "RUN pip2 install MySQL_python --no-cache-dir"
    it "pip3 --no-cache-dir used" $ do
      ruleCatchesNot "DL3042" "RUN pip3 install --no-cache-dir MySQL_python"
      onBuildRuleCatchesNot "DL3042" "RUN pip3 install --no-cache-dir MySQL_python"
    it "pip --no-cache-dir used" $ do
      ruleCatchesNot "DL3042" "RUN pip install MySQL_python --no-cache-dir"
      onBuildRuleCatchesNot "DL3042" "RUN pip install MySQL_python --no-cache-dir"
    it "don't match on pipx" $ do
      ruleCatchesNot "DL3042" "RUN pipx install software"
      onBuildRuleCatchesNot "DL3042" "Run pipx install software"
    it "don't match on pipenv" $ do
      ruleCatchesNot "DL3042" "RUN pipenv install library"
      onBuildRuleCatchesNot "DL3042" "RUN pipenv install library"
