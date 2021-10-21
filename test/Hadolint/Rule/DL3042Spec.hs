module Hadolint.Rule.DL3042Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

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

    -- ENV won't work with the onBuildRuleCatches[Not] wrapper:
    it "respect ENV PIP_NO_CACHE_DIR with truthy values" $ do
      ruleCatchesNot "DL3042" "ENV PIP_NO_CACHE_DIR=1\nRUN pip install MySQL_python"
      ruleCatchesNot "DL3042" "ENV PIP_NO_CACHE_DIR=on\nRUN pip install MySQL_python"
      ruleCatchesNot "DL3042" "ENV PIP_NO_CACHE_DIR=yes\nRUN pip install MySQL_python"
      ruleCatchesNot "DL3042" "ENV PIP_NO_CACHE_DIR=true\nRUN pip install MySQL_python"
    it "respect RUN PIP_NO_CACHE_DIR=... with truthy values" $ do
      ruleCatchesNot "DL3042" "RUN PIP_NO_CACHE_DIR=1 pip install MySQL_python"
      ruleCatchesNot "DL3042" "RUN PIP_NO_CACHE_DIR=on pip install MySQL_python"
      ruleCatchesNot "DL3042" "RUN PIP_NO_CACHE_DIR=yes pip install MySQL_python"
      ruleCatchesNot "DL3042" "RUN PIP_NO_CACHE_DIR=true pip install MySQL_python"
    it "respect RUN export PIP_NO_CACHE_DIR=... with truthy values" $ do
      ruleCatchesNot "DL3042" "RUN export PIP_NO_CACHE_DIR=1 && pip install MySQL_python"
      ruleCatchesNot "DL3042" "RUN export PIP_NO_CACHE_DIR=on && pip install MySQL_python"
      ruleCatchesNot "DL3042" "RUN export PIP_NO_CACHE_DIR=yes && pip install MySQL_python"
      ruleCatchesNot "DL3042" "RUN export PIP_NO_CACHE_DIR=true && pip install MySQL_python"
    it "respect ENV PIP_NO_CACHE_DIR with falsy values" $ do
      ruleCatches "DL3042" "ENV PIP_NO_CACHE_DIR=0\nRUN pip install MySQL_python"
      ruleCatches "DL3042" "ENV PIP_NO_CACHE_DIR=off\nRUN pip install MySQL_python"
      ruleCatches "DL3042" "ENV PIP_NO_CACHE_DIR=no\nRUN pip install MySQL_python"
      ruleCatches "DL3042" "ENV PIP_NO_CACHE_DIR=false\nRUN pip install MySQL_python"
    it "respect RUN PIP_NO_CACHE_DIR=... with falsy values" $ do
      ruleCatches "DL3042" "RUN PIP_NO_CACHE_DIR=0 pip install MySQL_python"
      ruleCatches "DL3042" "RUN PIP_NO_CACHE_DIR=off pip install MySQL_python"
      ruleCatches "DL3042" "RUN PIP_NO_CACHE_DIR=no pip install MySQL_python"
      ruleCatches "DL3042" "RUN PIP_NO_CACHE_DIR=false pip install MySQL_python"
    it "respect RUN export PIP_NO_CACHE_DIR=... with falsy values" $ do
      ruleCatches "DL3042" "RUN export PIP_NO_CACHE_DIR=0 && pip install MySQL_python"
      ruleCatches "DL3042" "RUN export PIP_NO_CACHE_DIR=off && pip install MySQL_python"
      ruleCatches "DL3042" "RUN export PIP_NO_CACHE_DIR=no && pip install MySQL_python"
      ruleCatches "DL3042" "RUN export PIP_NO_CACHE_DIR=false && pip install MySQL_python"

    it "don't trigger if PIP_NO_CACHE_DIR is inherited" $
      let dockerFile = Text.unlines
            [ "FROM debian:buster as base",
              "ENV PIP_NO_CACHE_DIR=1",
              "FROM base",
              "RUN pip install six"
            ]
       in do
        ruleCatchesNot "DL3042" dockerFile
    it "trigger if PIP_NO_CACHE_DIR is not inherited" $
      let dockerFile = Text.unlines
            [ "FROM debian:buster as base",
              "ENV PIP_NO_CACHE_DIR=1",
              "FROM debian:buster",
              "RUN pip install six"
            ]
       in do
        ruleCatches "DL3042" dockerFile
