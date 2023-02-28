module Hadolint.Rule.DL3013Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3013 - Pin versions in pip." $ do
    it "pip2 version not pinned" $ do
      ruleCatches "DL3013" "RUN pip2 install MySQL_python"
      onBuildRuleCatches "DL3013" "RUN pip2 install MySQL_python"
    it "pip3 version not pinned" $ do
      ruleCatches "DL3013" "RUN pip3 install MySQL_python"
      onBuildRuleCatches "DL3013" "RUN pip2 install MySQL_python"
    it "pip3 version pinned" $ do
      ruleCatchesNot "DL3013" "RUN pip3 install MySQL_python==1.2.2"
      onBuildRuleCatchesNot "DL3013" "RUN pip3 install MySQL_python==1.2.2"
    it "pip3 install from local package" $ do
      ruleCatchesNot "DL3013" "RUN pip3 install mypkg.whl"
      ruleCatchesNot "DL3013" "RUN pip3 install mypkg.tar.gz"
      onBuildRuleCatchesNot "DL3013" "RUN pip3 install mypkg.whl"
      onBuildRuleCatchesNot "DL3013" "RUN pip3 install mypkg.tar.gz"
    it "pip install requirements" $ do
      ruleCatchesNot "DL3013" "RUN pip install -r requirements.txt"
      onBuildRuleCatchesNot "DL3013" "RUN pip install -r requirements.txt"
    it "pip install requirements with long flag" $ do
      ruleCatchesNot "DL3013" "RUN pip install --requirement requirements.txt"
      onBuildRuleCatchesNot "DL3013" "RUN pip install --requirement requirements.txt"
    it "pip install use setup.py" $ do
      ruleCatchesNot "DL3013" "RUN pip install ."
      onBuildRuleCatchesNot "DL3013" "RUN pip install ."
    it "pip version not pinned" $ do
      ruleCatches "DL3013" "RUN pip install MySQL_python"
      onBuildRuleCatches "DL3013" "RUN pip install MySQL_python"
    it "pip version pinned" $ do
      ruleCatchesNot "DL3013" "RUN pip install MySQL_python==1.2.2"
      onBuildRuleCatchesNot "DL3013" "RUN pip install MySQL_python==1.2.2"
    it "pip version pinned with ~= operator" $ do
      ruleCatchesNot "DL3013" "RUN pip install MySQL_python~=1.2.2"
      onBuildRuleCatchesNot "DL3013" "RUN pip install MySQL_python~=1.2.2"
    it "pip version pinned with === operator" $ do
      ruleCatchesNot "DL3013" "RUN pip install MySQL_python===1.2.2"
      onBuildRuleCatchesNot "DL3013" "RUN pip install MySQL_python===1.2.2"
    it "pip version pinned with flag --ignore-installed" $ do
      ruleCatchesNot "DL3013" "RUN pip install --ignore-installed MySQL_python==1.2.2"
      onBuildRuleCatchesNot "DL3013" "RUN pip install --ignore-installed MySQL_python==1.2.2"
    it "pip version pinned with flag --build" $ do
      ruleCatchesNot "DL3013" "RUN pip3 install --build /opt/yamllint yamllint==1.20.0"
      onBuildRuleCatchesNot "DL3013" "RUN pip3 install --build /opt/yamllint yamllint==1.20.0"
    it "pip version pinned with flag --prefix" $ do
      ruleCatchesNot "DL3013" "RUN pip3 install --prefix /opt/yamllint yamllint==1.20.0"
      onBuildRuleCatchesNot "DL3013" "RUN pip3 install --prefix /opt/yamllint yamllint==1.20.0"
    it "pip version pinned with flag --root" $ do
      ruleCatchesNot "DL3013" "RUN pip3 install --root /opt/yamllint yamllint==1.20.0"
      onBuildRuleCatchesNot "DL3013" "RUN pip3 install --root /opt/yamllint yamllint==1.20.0"
    it "pip version pinned with flag --target" $ do
      ruleCatchesNot "DL3013" "RUN pip3 install --target /opt/yamllint yamllint==1.20.0"
      onBuildRuleCatchesNot "DL3013" "RUN pip3 install --target /opt/yamllint yamllint==1.20.0"
    it "pip version pinned with flag --trusted-host" $ do
      ruleCatchesNot "DL3013" "RUN pip3 install --trusted-host host example==1.2.2"
      onBuildRuleCatchesNot "DL3013" "RUN pip3 install --trusted-host host example==1.2.2"
    it "pip version pinned with python -m" $ do
      ruleCatchesNot "DL3013" "RUN python -m pip install example==1.2.2"
      onBuildRuleCatchesNot "DL3013" "RUN python -m pip install example==1.2.2"
    it "pip version not pinned with python -m" $ do
      ruleCatches "DL3013" "RUN python -m pip install example"
      onBuildRuleCatches "DL3013" "RUN python -m pip install --index-url url example"
    it "pip install git" $ do
      ruleCatchesNot
        "DL3013"
        "RUN pip install git+https://github.com/rtfd/r-ext.git@0.6-alpha#egg=r-ext"
      onBuildRuleCatchesNot
        "DL3013"
        "RUN pip install git+https://github.com/rtfd/r-ext.git@0.6-alpha#egg=r-ext"
      ruleCatchesNot
        "DL3013"
        "RUN pip install git+ssh://github.com/rtfd/r-ext.git@0.6-alpha#egg=r-ext"
      onBuildRuleCatchesNot
        "DL3013"
        "RUN pip install git+ssh://github.com/rtfd/r-ext.git@0.6-alpha#egg=r-ext"
    it "pip install unversioned git" $ do
      ruleCatches
        "DL3013"
        "RUN pip install git+https://github.com/rtfd/read-ext.git#egg=read-ext"
      onBuildRuleCatches
        "DL3013"
        "RUN pip install git+https://github.com/rtfd/read-ext.git#egg=read-ext"
      ruleCatches
        "DL3013"
        "RUN pip install git+ssh://github.com/rtfd/read-ext.git#egg=read-ext"
      onBuildRuleCatches
        "DL3013"
        "RUN pip install git+ssh://github.com/rtfd/read-ext.git#egg=read-ext"
    it "pip install local dir" $ do
      ruleCatchesNot
        "DL3013"
        "RUN pip install foo/bar"
      onBuildRuleCatchesNot
        "DL3013"
        "RUN pip install foo/bar"
    it "pip install https url package" $ do
      ruleCatchesNot
        "DL3013"
        "RUN pip install https://foo.bar/baz.zip"
      onBuildRuleCatchesNot
        "DL3013"
        "RUN pip install https://foo.bar/baz.zip"
    it "pip install upper bound" $ do
      ruleCatchesNot "DL3013" "RUN pip install 'alabaster>=0.7'"
      onBuildRuleCatchesNot "DL3013" "RUN pip install 'alabaster>=0.7'"
    it "pip install lower bound" $ do
      ruleCatchesNot "DL3013" "RUN pip install 'alabaster<0.7'"
      onBuildRuleCatchesNot "DL3013" "RUN pip install 'alabaster<0.7'"
    it "pip install excluded version" $ do
      ruleCatchesNot "DL3013" "RUN pip install 'alabaster!=0.7'"
      onBuildRuleCatchesNot "DL3013" "RUN pip install 'alabaster!=0.7'"
    it "pip install user directory" $ do
      ruleCatchesNot "DL3013" "RUN pip install MySQL_python==1.2.2 --user"
      onBuildRuleCatchesNot "DL3013" "RUN pip install MySQL_python==1.2.2 --user"
    it "pip install no pip version check" $ do
      ruleCatchesNot
        "DL3013"
        "RUN pip install MySQL_python==1.2.2 --disable-pip-version-check"
      onBuildRuleCatchesNot
        "DL3013"
        "RUN pip install MySQL_python==1.2.2 --disable-pip-version-check"
    it "pip install --index-url" $ do
      ruleCatchesNot
        "DL3013"
        "RUN pip install --index-url https://eg.com/foo foobar==1.0.0"
      onBuildRuleCatchesNot
        "DL3013"
        "RUN pip install --index-url https://eg.com/foo foobar==1.0.0"
    it "pip install index-url with -i flag" $ do
      ruleCatchesNot
        "DL3013"
        "RUN pip install --index-url https://eg.com/foo foobar==1.0.0"
      onBuildRuleCatchesNot
        "DL3013"
        "RUN pip install --index-url https://eg.com/foo foobar==1.0.0"
    it "pip install --index-url with --extra-index-url" $ do
      ruleCatchesNot
        "DL3013"
        "RUN pip install --index-url https://eg.com/foo --extra-index-url https://ex-eg.io/foo foobar==1.0.0"
      onBuildRuleCatchesNot
        "DL3013"
        "RUN pip install --index-url https://eg.com/foo --extra-index-url https://ex-eg.io/foo foobar==1.0.0"
    it "pip install no cache dir" $ do
      ruleCatchesNot "DL3013" "RUN pip install MySQL_python==1.2.2 --no-cache-dir"
      onBuildRuleCatchesNot "DL3013" "RUN pip install MySQL_python==1.2.2 --no-cache-dir"
    it "pip install constraints file - long version argument" $ do
      ruleCatchesNot "DL3013" "RUN pip install pykafka --constraint http://foo.bar.baz"
      onBuildRuleCatchesNot "DL3013" "RUN pip install pykafka --constraint http://foo.bar.baz"
    it "pip install constraints file - short version argument" $ do
      ruleCatchesNot "DL3013" "RUN pip install pykafka -c http://foo.bar.baz"
      onBuildRuleCatchesNot "DL3013" "RUN pip install pykafka -c http://foo.bar.baz"
    it "pip install --index-url with --extra-index-url with basic auth" $ do
      ruleCatchesNot
        "DL3013"
        "RUN pip install --index-url https://user:pass@eg.com/foo --extra-index-url https://user:pass@ex-eg.io/foo foobar==1.0.0"
      onBuildRuleCatchesNot
        "DL3013"
        "RUN pip install --index-url https://user:pass@eg.com/foo --extra-index-url https://user:pass@ex-eg.io/foo foobar==1.0.0"
    it "pipenv is not pip" $ do
      ruleCatchesNot "DL3013" "RUN pipenv install black"
      onBuildRuleCatchesNot "DL3013" "RUN pipenv install black"
