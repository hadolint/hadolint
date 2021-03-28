module DL3028 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3028 - Pin versions in gem install." $
    describe "version pinning" $ do
      describe "i" $ do
        it "unpinned" $ do
          ruleCatches "DL3028" "RUN gem i bundler"
          onBuildRuleCatches "DL3028" "RUN gem i bundler"
        it "pinned" $ do
          ruleCatchesNot "DL3028" "RUN gem i bundler:1"
          onBuildRuleCatchesNot "DL3028" "RUN gem i bundler:1"
        it "multi" $ do
          ruleCatches "DL3028" "RUN gem i bunlder:1 nokogiri"
          onBuildRuleCatches "DL3028" "RUN gem i bunlder:1 nokogiri"
          ruleCatchesNot "DL3028" "RUN gem i bunlder:1 nokogirii:1"
          onBuildRuleCatchesNot "DL3028" "RUN gem i bunlder:1 nokogiri:1"
      describe "install" $ do
        it "unpinned" $ do
          ruleCatches "DL3028" "RUN gem install bundler"
          onBuildRuleCatches "DL3028" "RUN gem install bundler"
        it "pinned" $ do
          ruleCatchesNot "DL3028" "RUN gem install bundler:1"
          onBuildRuleCatchesNot "DL3028" "RUN gem install bundler:1"
        it "does not warn on -v" $ do
          ruleCatchesNot "DL3028" "RUN gem install bundler -v '2.0.1'"
          onBuildRuleCatchesNot "DL3028" "RUN gem install bundler -v '2.0.1'"
        it "does not warn on --version without =" $ do
          ruleCatchesNot "DL3028" "RUN gem install bundler --version '2.0.1'"
          onBuildRuleCatchesNot "DL3028" "RUN gem install bundler --version '2.0.1'"
        it "does not warn on --version with =" $ do
          ruleCatchesNot "DL3028" "RUN gem install bundler --version='2.0.1'"
          onBuildRuleCatchesNot "DL3028" "RUN gem install bundler --version='2.0.1'"
        it "does not warn on extra flags" $ do
          ruleCatchesNot "DL3028" "RUN gem install bundler:2.0.1 -- --use-system-libraries=true"
          onBuildRuleCatchesNot "DL3028" "RUN gem install bundler:2.0.1 -- --use-system-libraries=true"
