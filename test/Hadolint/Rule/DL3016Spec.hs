module Hadolint.Rule.DL3016Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def
  describe "DL3016 - Pin versions in npm." $ do
    it "version pinned in package.json" $ do
      ruleCatchesNot "DL3016" "RUN npm install"
      onBuildRuleCatchesNot "DL3016" "RUN npm install"
    it "version pinned in package.json with arguments" $ do
      ruleCatchesNot "DL3016" "RUN npm install --progress=false"
      onBuildRuleCatchesNot "DL3016" "RUN npm install --progress=false"
    it "version pinned" $ do
      ruleCatchesNot "DL3016" "RUN npm install express@4.1.1"
      onBuildRuleCatchesNot "DL3016" "RUN npm install express@4.1.1"
    it "version pinned with scope" $ do
      ruleCatchesNot "DL3016" "RUN npm install @myorg/privatepackage@\">=0.1.0\""
      onBuildRuleCatchesNot "DL3016" "RUN npm install @myorg/privatepackage@\">=0.1.0\""
    it "version pinned multiple packages" $ do
      ruleCatchesNot "DL3016" "RUN npm install express@\"4.1.1\" sax@0.1.1"
      onBuildRuleCatchesNot "DL3016" "RUN npm install express@\"4.1.1\" sax@0.1.1"
    it "version pinned with --global" $ do
      ruleCatchesNot "DL3016" "RUN npm install --global express@\"4.1.1\""
      onBuildRuleCatchesNot "DL3016" "RUN npm install --global express@\"4.1.1\""
    it "version pinned with -g" $ do
      ruleCatchesNot "DL3016" "RUN npm install -g express@\"4.1.1\""
      onBuildRuleCatchesNot "DL3016" "RUN npm install -g express@\"4.1.1\""
    it "version does not have to be pinned for tarball suffix .tar" $ do
      ruleCatchesNot "DL3016" "RUN npm install package-v1.2.3.tar"
      onBuildRuleCatchesNot "DL3016" "RUN npm install package-v1.2.3.tar"
    it "version does not have to be pinned for tarball suffix .tar.gz" $ do
      ruleCatchesNot "DL3016" "RUN npm install package-v1.2.3.tar.gz"
      onBuildRuleCatchesNot "DL3016" "RUN npm install package-v1.2.3.tar.gz"
    it "version does not have to be pinned for tarball suffix .tgz" $ do
      ruleCatchesNot "DL3016" "RUN npm install package-v1.2.3.tgz"
      onBuildRuleCatchesNot "DL3016" "RUN npm install package-v1.2.3.tgz"
    it "version does not have to be pinned for folder - absolute path" $ do
      ruleCatchesNot "DL3016" "RUN npm install /folder"
      onBuildRuleCatchesNot "DL3016" "RUN npm install /folder"
    it "version does not have to be pinned for folder - relative path from current folder" $ do
      ruleCatchesNot "DL3016" "RUN npm install ./folder"
      onBuildRuleCatchesNot "DL3016" "RUN npm install ./folder"
    it "version does not have to be pinned for folder - relative path to parent folder" $ do
      ruleCatchesNot "DL3016" "RUN npm install ../folder"
      onBuildRuleCatchesNot "DL3016" "RUN npm install ../folder"
    it "version does not have to be pinned for folder - relative path from home" $ do
      ruleCatchesNot "DL3016" "RUN npm install ~/folder"
      onBuildRuleCatchesNot "DL3016" "RUN npm install ~/folder"
    it "commit pinned for git+ssh" $ do
      ruleCatchesNot
        "DL3016"
        "RUN npm install git+ssh://git@github.com:npm/npm.git#v1.0.27"
      onBuildRuleCatchesNot
        "DL3016"
        "RUN npm install git+ssh://git@github.com:npm/npm.git#v1.0.27"
    it "commit pinned for git+http" $ do
      ruleCatchesNot
        "DL3016"
        "RUN npm install git+http://isaacs@github.com/npm/npm#semver:^5.0"
      onBuildRuleCatchesNot
        "DL3016"
        "RUN npm install git+http://isaacs@github.com/npm/npm#semver:^5.0"
    it "commit pinned for git+https" $ do
      ruleCatchesNot
        "DL3016"
        "RUN npm install git+https://isaacs@github.com/npm/npm.git#v1.0.27"
      onBuildRuleCatchesNot
        "DL3016"
        "RUN npm install git+https://isaacs@github.com/npm/npm.git#v1.0.27"
    it "commit pinned for git" $ do
      ruleCatchesNot
        "DL3016"
        "RUN npm install git://github.com/npm/npm.git#v1.0.27"
      onBuildRuleCatchesNot
        "DL3016"
        "RUN npm install git://github.com/npm/npm.git#v1.0.27"
    it "npm run install is fine" $ do
      ruleCatchesNot
        "DL3016"
        "RUN npm run --crazy install"
      onBuildRuleCatchesNot
        "DL3016"
        "RUN npm run --crazy install"

    --version range is not supported
    it "version pinned with scope" $ do
      ruleCatchesNot "DL3016" "RUN npm install @myorg/privatepackage@\">=0.1.0 <0.2.0\""
      onBuildRuleCatchesNot "DL3016" "RUN npm install @myorg/privatepackage@\">=0.1.0 <0.2.0\""
    it "version not pinned" $ do
      ruleCatches "DL3016" "RUN npm install express"
      onBuildRuleCatches "DL3016" "RUN npm install express"
    it "version not pinned with scope" $ do
      ruleCatches "DL3016" "RUN npm install @myorg/privatepackage"
      onBuildRuleCatches "DL3016" "RUN npm install @myorg/privatepackage"
    it "version not pinned multiple packages" $ do
      ruleCatches "DL3016" "RUN npm install express sax@0.1.1"
      onBuildRuleCatches "DL3016" "RUN npm install express sax@0.1.1"
    it "version not pinned with --global" $ do
      ruleCatches "DL3016" "RUN npm install --global express"
      onBuildRuleCatches "DL3016" "RUN npm install --global express"
    it "commit not pinned for git+ssh" $ do
      ruleCatches "DL3016" "RUN npm install git+ssh://git@github.com:npm/npm.git"
      onBuildRuleCatches "DL3016" "RUN npm install git+ssh://git@github.com:npm/npm.git"
    it "commit not pinned for git+http" $ do
      ruleCatches "DL3016" "RUN npm install git+http://isaacs@github.com/npm/npm"
      onBuildRuleCatches "DL3016" "RUN npm install git+http://isaacs@github.com/npm/npm"
    it "commit not pinned for git+https" $ do
      ruleCatches
        "DL3016"
        "RUN npm install git+https://isaacs@github.com/npm/npm.git"
      onBuildRuleCatches
        "DL3016"
        "RUN npm install git+https://isaacs@github.com/npm/npm.git"
    it "commit not pinned for git" $ do
      ruleCatches "DL3016" "RUN npm install git://github.com/npm/npm.git"
      onBuildRuleCatches "DL3016" "RUN npm install git://github.com/npm/npm.git"
    it "don't fire on loglevel flag" $ do
      ruleCatchesNot "DL3016" "RUN npm install --loglevel verbose sax@0.1.1"
      onBuildRuleCatchesNot "DL3016" "RUN npm install --loglevel verbose sax@0.1.1"
