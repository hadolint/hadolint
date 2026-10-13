module Hadolint.Rule.DL3069Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def
  describe "DL3069 - Pin versions in composer." $ do
    describe "pinned requirements are accepted" $ do
      it "caret constraint" $ do
        ruleCatchesNot "DL3069" "RUN composer require vendor/package:^4.2"
        onBuildRuleCatchesNot "DL3069" "RUN composer require vendor/package:^4.2"
      it "tilde constraint" $ do
        ruleCatchesNot "DL3069" "RUN composer require vendor/package:~4.2"
        onBuildRuleCatchesNot "DL3069" "RUN composer require vendor/package:~4.2"
      it "exact version" $ do
        ruleCatchesNot "DL3069" "RUN composer require vendor/package:4.5.0"
        onBuildRuleCatchesNot "DL3069" "RUN composer require vendor/package:4.5.0"
      it "version as a separate argument" $ do
        ruleCatchesNot "DL3069" "RUN composer require vendor/package 4.5.0"
        onBuildRuleCatchesNot "DL3069" "RUN composer require vendor/package 4.5.0"
      it "stability-qualified version" $ do
        ruleCatchesNot "DL3069" "RUN composer require vendor/package:4.5@dev"
        onBuildRuleCatchesNot "DL3069" "RUN composer require vendor/package:4.5@dev"
      it "dev branch pinned to a commit" $ do
        ruleCatchesNot
          "DL3069"
          "RUN composer require vendor/package:dev-master#2eb0c0978d290a1c45346a1955188929cb4e5db7"
        onBuildRuleCatchesNot
          "DL3069"
          "RUN composer require vendor/package:dev-master#2eb0c0978d290a1c45346a1955188929cb4e5db7"

    describe "update subcommand is checked too" $ do
      it "update with a constraint is pinned" $ do
        ruleCatchesNot "DL3069" "RUN composer update vendor/package:^4.2"
        onBuildRuleCatchesNot "DL3069" "RUN composer update vendor/package:^4.2"
      it "update without a constraint is flagged" $ do
        ruleCatches "DL3069" "RUN composer update vendor/package"
        onBuildRuleCatches "DL3069" "RUN composer update vendor/package"

    describe "unpinned requirements are flagged" $ do
      it "no constraint" $ do
        ruleCatches "DL3069" "RUN composer require vendor/package"
        onBuildRuleCatches "DL3069" "RUN composer require vendor/package"
      it "wildcard constraint" $ do
        ruleCatches "DL3069" "RUN composer require vendor/package:*"
        onBuildRuleCatches "DL3069" "RUN composer require vendor/package:*"
      it "one pinned and one unpinned package" $ do
        ruleCatches "DL3069" "RUN composer require vendor/a:^1.0 vendor/b"
        onBuildRuleCatches "DL3069" "RUN composer require vendor/a:^1.0 vendor/b"

    describe "multiple packages" $ do
      it "all pinned" $ do
        ruleCatchesNot "DL3069" "RUN composer require vendor/a:^1.0 vendor/b:~2.3"
        onBuildRuleCatchesNot "DL3069" "RUN composer require vendor/a:^1.0 vendor/b:~2.3"

    describe "flags do not affect detection" $ do
      it "leading --dev flag with a pinned package" $ do
        ruleCatchesNot "DL3069" "RUN composer require --dev vendor/package:^4.2"
        onBuildRuleCatchesNot "DL3069" "RUN composer require --dev vendor/package:^4.2"
      it "leading --dev flag with an unpinned package" $ do
        ruleCatches "DL3069" "RUN composer require --dev vendor/package"
        onBuildRuleCatches "DL3069" "RUN composer require --dev vendor/package"

    describe "other composer subcommands are ignored" $ do
      it "remove is not checked" $ do
        ruleCatchesNot "DL3069" "RUN composer remove vendor/package"
        onBuildRuleCatchesNot "DL3069" "RUN composer remove vendor/package"
      it "install is not checked" $ do
        ruleCatchesNot "DL3069" "RUN composer install"
        onBuildRuleCatchesNot "DL3069" "RUN composer install"
