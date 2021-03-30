module DL4005 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL4005 - Use `SHELL` to change the default shell." $ do
    it "RUN ln" $ do
      ruleCatches "DL4005" "RUN ln -sfv /bin/bash /bin/sh"
      onBuildRuleCatches "DL4005" "RUN ln -sfv /bin/bash /bin/sh"
    it "RUN ln with unrelated symlinks" $ do
      ruleCatchesNot "DL4005" "RUN ln -sf /bin/true /sbin/initctl"
      onBuildRuleCatchesNot "DL4005" "RUN ln -sf /bin/true /sbin/initctl"
    it "RUN ln with multiple acceptable commands" $ do
      ruleCatchesNot "DL4005" "RUN ln -s foo bar && unrelated && something_with /bin/sh"
      onBuildRuleCatchesNot "DL4005" "RUN ln -s foo bar && unrelated && something_with /bin/sh"
