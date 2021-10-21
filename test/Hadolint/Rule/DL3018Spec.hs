module Hadolint.Rule.DL3018Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3018 - Pin versions in `apk add`." $ do
    it "apk add version pinning single" $ do
      ruleCatches "DL3018" "RUN apk add flex"
      onBuildRuleCatches "DL3018" "RUN apk add flex"
    it "apk add no version pinning single" $ do
      ruleCatchesNot "DL3018" "RUN apk add flex=2.6.4-r1"
      onBuildRuleCatchesNot "DL3018" "RUN apk add flex=2.6.4-r1"
    it "apk add version pinned chained" $
      let dockerFile =
            [ "RUN apk add --no-cache flex=2.6.4-r1 \\",
              " && pip install -r requirements.txt"
            ]
       in do
            ruleCatchesNot "DL3018" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3018" $ Text.unlines dockerFile
    it "apk add version pinned regression" $
      let dockerFile =
            [ "RUN apk add --no-cache \\",
              "flex=2.6.4-r1 \\",
              "libffi=3.2.1-r3 \\",
              "python2=2.7.13-r1 \\",
              "libbz2=1.0.6-r5"
            ]
       in do
            ruleCatchesNot "DL3018" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3018" $ Text.unlines dockerFile
    it "apk add version pinned regression - one missed" $
      let dockerFile =
            [ "RUN apk add --no-cache \\",
              "flex=2.6.4-r1 \\",
              "libffi \\",
              "python2=2.7.13-r1 \\",
              "libbz2=1.0.6-r5"
            ]
       in do
            ruleCatches "DL3018" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3018" $ Text.unlines dockerFile
    it "apk add virtual package" $
      let dockerFile =
            [ "RUN apk add \\",
              "--virtual build-dependencies \\",
              "python-dev=1.1.1 build-base=2.2.2 wget=3.3.3 \\",
              "&& pip install -r requirements.txt \\",
              "&& python setup.py install \\",
              "&& apk del build-dependencies"
            ]
       in do
            ruleCatchesNot "DL3018" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3018" $ Text.unlines dockerFile
    it "apk add with repository without equal sign" $
      let dockerFile =
            [ "RUN apk add --no-cache \\",
              "--repository https://nl.alpinelinux.org/alpine/edge/testing \\",
              "flow=0.78.0-r0"
            ]
       in do
            ruleCatchesNot "DL3018" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3018" $ Text.unlines dockerFile
    it "apk add with repository with equal sign" $
      let dockerFile =
            [ "RUN apk add --no-cache \\",
              "--repository=https://nl.alpinelinux.org/alpine/edge/testing \\",
              "flow=0.78.0-r0"
            ]
       in do
            ruleCatchesNot "DL3018" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3018" $ Text.unlines dockerFile
    it "apk add with repository (-X) without equal sign" $
      let dockerFile =
            [ "RUN apk add --no-cache \\",
              "-X https://nl.alpinelinux.org/alpine/edge/testing \\",
              "flow=0.78.0-r0"
            ]
       in do
            ruleCatchesNot "DL3018" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3018" $ Text.unlines dockerFile

    it "don't trigger when installing from .apk file" $ do
      ruleCatchesNot "DL3018" "RUN apk add mypackage-1.1.1.apk"
      onBuildRuleCatchesNot "DL3018" "RUN apk add mypackage-1.1.1.apk"
