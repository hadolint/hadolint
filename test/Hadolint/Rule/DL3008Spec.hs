module Hadolint.Rule.DL3008Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3008 - Pin versions in apt-get install." $ do
    it "apt-get version pinning" $ do
      ruleCatches "DL3008" "RUN apt-get update && apt-get install python"
      onBuildRuleCatches "DL3008" "RUN apt-get update && apt-get install python"
    it "apt-get pinned chained" $
      let dockerFile =
            [ "RUN apt-get update \\",
              " && apt-get -yqq --no-install-recommends install nodejs=0.10 \\",
              " && rm -rf /var/lib/apt/lists/*"
            ]
       in do
            ruleCatchesNot "DL3008" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3008" $ Text.unlines dockerFile

    it "apt-get pinned regression" $
      let dockerFile =
            [ "RUN apt-get update && apt-get install --no-install-recommends -y \\",
              "python-demjson=2.2.2* \\",
              "wget=1.16.1* \\",
              "git=1:2.5.0* \\",
              "ruby=1:2.1.*"
            ]
       in do
            ruleCatchesNot "DL3008" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3008" $ Text.unlines dockerFile
    it "apt-get version" $ do
      ruleCatchesNot "DL3008" "RUN apt-get install -y python=1.2.2"
      onBuildRuleCatchesNot "DL3008" "RUN apt-get install -y python=1.2.2"
    it "apt-get version" $ do
      ruleCatchesNot "DL3008" "RUN apt-get install ./wkhtmltox_0.12.5-1.bionic_amd64.deb"
      onBuildRuleCatchesNot "DL3008" "RUN apt-get install ./wkhtmltox_0.12.5-1.bionic_amd64.deb"
    it "apt-get pinned" $ do
      ruleCatchesNot
        "DL3008"
        "RUN apt-get -y --no-install-recommends install nodejs=0.10"
      onBuildRuleCatchesNot
        "DL3008"
        "RUN apt-get -y --no-install-recommends install nodejs=0.10"
    it "apt-get tolerate target-release" $
      let dockerFile =
            [ "RUN set -e &&\\",
              " echo \"deb http://http.debian.net/debian jessie-backports main\" \
              \> /etc/apt/sources.list.d/jessie-backports.list &&\\",
              " apt-get update &&\\",
              " apt-get install -y --no-install-recommends -t jessie-backports \
              \openjdk-8-jdk=8u131-b11-1~bpo8+1 &&\\",
              " rm -rf /var/lib/apt/lists/*"
            ]
       in do
            ruleCatchesNot "DL3008" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3008" $ Text.unlines dockerFile
