module Hadolint.Rule.DL3042Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3042 - Use BuildKit cache mount for pip." $ do
    it "warn: pip2 install without cache mount" $ do
      ruleCatches "DL3042" "RUN pip2 install MySQL_python"
      onBuildRuleCatches "DL3042" "RUN pip2 install MySQL_python"
    it "warn: pip3 install without cache mount" $ do
      ruleCatches "DL3042" "RUN pip3 install MySQL_python"
      onBuildRuleCatches "DL3042" "RUN pip3 install MySQL_python"
    it "warn: pip install without cache mount" $ do
      ruleCatches "DL3042" "RUN pip install MySQL_python"
      onBuildRuleCatches "DL3042" "RUN pip install MySQL_python"

    it "warn: --no-cache-dir no longer suppresses" $ do
      ruleCatches "DL3042" "RUN pip2 install MySQL_python --no-cache-dir"
      onBuildRuleCatches "DL3042" "RUN pip2 install MySQL_python --no-cache-dir"
    it "warn: --no-cache-dir on pip3 no longer suppresses" $ do
      ruleCatches "DL3042" "RUN pip3 install --no-cache-dir MySQL_python"
      onBuildRuleCatches "DL3042" "RUN pip3 install --no-cache-dir MySQL_python"
    it "warn: --no-cache-dir on pip no longer suppresses" $ do
      ruleCatches "DL3042" "RUN pip install MySQL_python --no-cache-dir"
      onBuildRuleCatches "DL3042" "RUN pip install MySQL_python --no-cache-dir"

    it "don't match on pipx" $ do
      ruleCatchesNot "DL3042" "RUN pipx install software"
      onBuildRuleCatchesNot "DL3042" "RUN pipx install software"
    it "don't match on pipenv" $ do
      ruleCatchesNot "DL3042" "RUN pipenv install library"
      onBuildRuleCatchesNot "DL3042" "RUN pipenv install library"

    it "warn: PIP_NO_CACHE_DIR env no longer suppresses" $ do
      ruleCatches "DL3042" "ENV PIP_NO_CACHE_DIR=1\nRUN pip install MySQL_python"
      ruleCatches "DL3042" "ENV PIP_NO_CACHE_DIR=on\nRUN pip install MySQL_python"
      ruleCatches "DL3042" "ENV PIP_NO_CACHE_DIR=yes\nRUN pip install MySQL_python"
      ruleCatches "DL3042" "ENV PIP_NO_CACHE_DIR=true\nRUN pip install MySQL_python"
    it "warn: inline PIP_NO_CACHE_DIR=1 no longer suppresses" $ do
      ruleCatches "DL3042" "RUN PIP_NO_CACHE_DIR=1 pip install MySQL_python"
    it "warn: falsy PIP_NO_CACHE_DIR still fires" $ do
      ruleCatches "DL3042" "ENV PIP_NO_CACHE_DIR=0\nRUN pip install MySQL_python"
      ruleCatches "DL3042" "ENV PIP_NO_CACHE_DIR=off\nRUN pip install MySQL_python"

    it "don't warn: cache mount in cache dir" $
      let line = "RUN --mount=type=cache,target=/root/.cache/pip pip install foobar"
      in do
        ruleCatchesNot "DL3042" line
        onBuildRuleCatchesNot "DL3042" line

    it "don't warn: tmpfs mount in cache dir" $
      let line = "RUN --mount=type=tmpfs,target=/root/.cache/pip pip install foobar"
      in do
        ruleCatchesNot "DL3042" line
        onBuildRuleCatchesNot "DL3042" line

    it "don't warn: cache mount with non-root user home" $
      let line = "RUN --mount=type=cache,target=/home/app/.cache/pip pip install foobar"
      in do
        ruleCatchesNot "DL3042" line
        onBuildRuleCatchesNot "DL3042" line
