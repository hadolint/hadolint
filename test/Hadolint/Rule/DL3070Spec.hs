module Hadolint.Rule.DL3070Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def
  describe "DL3070 - Pin versions in pie." $ do
    describe "pinned install is accepted" $ do
      it "caret constraint" $ do
        ruleCatchesNot "DL3070" "RUN pie install xdebug/xdebug:^3.4"
        onBuildRuleCatchesNot "DL3070" "RUN pie install xdebug/xdebug:^3.4"
      it "exact version" $ do
        ruleCatchesNot "DL3070" "RUN pie install xdebug/xdebug:3.4.1"
        onBuildRuleCatchesNot "DL3070" "RUN pie install xdebug/xdebug:3.4.1"
      it "constraint with stability" $ do
        ruleCatchesNot "DL3070" "RUN pie install xdebug/xdebug:^3.4@alpha"
        onBuildRuleCatchesNot "DL3070" "RUN pie install xdebug/xdebug:^3.4@alpha"
      it "bare stability constraint" $ do
        ruleCatchesNot "DL3070" "RUN pie install xdebug/xdebug:@alpha"
        onBuildRuleCatchesNot "DL3070" "RUN pie install xdebug/xdebug:@alpha"
      it "dev branch pinned to a commit" $ do
        ruleCatchesNot "DL3070" "RUN pie install foo/bar:dev-main#07f454ad797c3065"
        onBuildRuleCatchesNot "DL3070" "RUN pie install foo/bar:dev-main#07f454ad797c3065"

    describe "build and download subcommands are checked too" $ do
      it "build without a constraint is flagged" $ do
        ruleCatches "DL3070" "RUN pie build xdebug/xdebug"
        onBuildRuleCatches "DL3070" "RUN pie build xdebug/xdebug"
      it "build with a constraint is pinned" $ do
        ruleCatchesNot "DL3070" "RUN pie build xdebug/xdebug:^3.4"
        onBuildRuleCatchesNot "DL3070" "RUN pie build xdebug/xdebug:^3.4"
      it "download without a constraint is flagged" $ do
        ruleCatches "DL3070" "RUN pie download xdebug/xdebug"
        onBuildRuleCatches "DL3070" "RUN pie download xdebug/xdebug"
      it "download with a constraint is pinned" $ do
        ruleCatchesNot "DL3070" "RUN pie download xdebug/xdebug:^3.4"
        onBuildRuleCatchesNot "DL3070" "RUN pie download xdebug/xdebug:^3.4"

    describe "unpinned install is flagged" $ do
      it "no constraint" $ do
        ruleCatches "DL3070" "RUN pie install xdebug/xdebug"
        onBuildRuleCatches "DL3070" "RUN pie install xdebug/xdebug"
      it "one pinned and one unpinned package" $ do
        ruleCatches "DL3070" "RUN pie install foo/bar:^1.0 baz/qux"
        onBuildRuleCatches "DL3070" "RUN pie install foo/bar:^1.0 baz/qux"

    describe "multiple packages all pinned" $ do
      it "two pinned packages" $ do
        ruleCatchesNot "DL3070" "RUN pie install foo/bar:^1.0 baz/qux:~2.3"
        onBuildRuleCatchesNot "DL3070" "RUN pie install foo/bar:^1.0 baz/qux:~2.3"

    describe "project-wide install with no package is not flagged" $ do
      it "install with no argument" $ do
        ruleCatchesNot "DL3070" "RUN pie install"
        onBuildRuleCatchesNot "DL3070" "RUN pie install"
      it "install with only the --no-dev flag" $ do
        ruleCatchesNot "DL3070" "RUN pie install --no-dev"
        onBuildRuleCatchesNot "DL3070" "RUN pie install --no-dev"

    describe "flag values are not treated as packages" $ do
      it "with-php-config value with a pinned package" $ do
        ruleCatchesNot "DL3070" "RUN pie install --with-php-config=/usr/bin/php-config7.2 a/b:^3.4"
        onBuildRuleCatchesNot "DL3070" "RUN pie install --with-php-config=/bin/php-config a/b:^3.4"
      it "select value containing a slash does not mask an unpinned package" $ do
        ruleCatches "DL3070" "RUN pie install --select redis=phpredis/phpredis a/b"
        onBuildRuleCatches "DL3070" "RUN pie install --select redis=phpredis/phpredis a/b"

    describe "other pie subcommands are ignored" $ do
      it "info is not checked" $ do
        ruleCatchesNot "DL3070" "RUN pie info xdebug/xdebug"
        onBuildRuleCatchesNot "DL3070" "RUN pie info xdebug/xdebug"
      it "upgrade is not checked" $ do
        ruleCatchesNot "DL3070" "RUN pie upgrade"
        onBuildRuleCatchesNot "DL3070" "RUN pie upgrade"
