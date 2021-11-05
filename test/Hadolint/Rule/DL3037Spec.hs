module Hadolint.Rule.DL3037Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3037 - Specify version with `zypper install -y <package>[=]<version>`." $ do
    -- NOTE: In Haskell strings, '\' has to be escaped. And in shell commands, '>'
    -- and '<' have to be escaped. Hence the double escaping.
    it "not ok without zypper version pinning" $ do
      ruleCatches "DL3037" "RUN zypper install -y tomcat && zypper clean"
      onBuildRuleCatches "DL3037" "RUN zypper install -y tomcat && zypper clean"
    it "ok with different variants of zypper version pinning" $ do
      ruleCatchesNot "DL3037" "RUN zypper install -y tomcat=9.0.39 && zypper clean"
      ruleCatchesNot "DL3037" "RUN zypper install -y tomcat\\>=9.0 && zypper clean"
      ruleCatchesNot "DL3037" "RUN zypper install -y tomcat\\>9.0 && zypper clean"
      ruleCatchesNot "DL3037" "RUN zypper install -y tomcat\\<=9.0 && zypper clean"
      ruleCatchesNot "DL3037" "RUN zypper install -y tomcat\\<9.0 && zypper clean"
      ruleCatchesNot "DL3037" "RUN zypper install -y tomcat-9.0.39-1.rpm && zypper clean"
      onBuildRuleCatchesNot "DL3037" "RUN zypper install -y tomcat=9.0.39 && zypper clean"
      onBuildRuleCatchesNot "DL3037" "RUN zypper install -y tomcat\\>=9.0 && zypper clean"
      onBuildRuleCatchesNot "DL3037" "RUN zypper install -y tomcat\\>9.0 && zypper clean"
      onBuildRuleCatchesNot "DL3037" "RUN zypper install -y tomcat\\<=9.0 && zypper clean"
      onBuildRuleCatchesNot "DL3037" "RUN zypper install -y tomcat\\<9.0 && zypper clean"
      onBuildRuleCatchesNot "DL3037" "RUN zypper install -y tomcat-9.0.39-1.rpm && zypper clean"
