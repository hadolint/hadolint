module Hadolint.Rule.DL3003Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3003 - Use WORKDIR to switch to a directory." $ do
    it "ok using WORKDIR" $ ruleCatchesNot "DL3003" "WORKDIR /opt"
    it "not ok using cd" $ ruleCatches "DL3003" "RUN cd /opt"

    it "allows cd in a subshell" $
      ruleCatchesNot "DL3003" "RUN (cd /tmp/build && make install)"

    it "allows cd in a nested subshell" $
      ruleCatchesNot "DL3003" "RUN (echo build && (cd /tmp/build && make))"

    it "allows a subshell followed by cleanup" $
      ruleCatchesNot "DL3003" "RUN (cd /tmp/build && make install) && rm -rf /tmp/build"

    it "does not exempt cd outside a subshell" $
      ruleCatches "DL3003" "RUN (cd /tmp/build && make) && cd /opt"

    it "does not exempt cd before a subshell" $
      ruleCatches "DL3003" "RUN cd /opt && (cd /tmp/build && make)"

    it "does not exempt brace groups" $
      ruleCatches "DL3003" "RUN { cd /tmp/build && make; }"

    it "does not exempt a function body" $
      ruleCatches "DL3003" "RUN build() { cd /tmp/build && make; }; build"

    it "allows a subshell within a function" $
      ruleCatchesNot "DL3003" "RUN build() { (cd /tmp/build && make); }; build"

    it "does not exempt a command named in quoted text" $
      ruleCatches "DL3003" "RUN echo '(cd /tmp)' && cd /opt"

    it "allows builtin cd in a subshell" $
      ruleCatchesNot "DL3003" "RUN (builtin cd /tmp/build && make)"

    it "does not exempt builtin cd outside a subshell" $
      ruleCatches "DL3003" "RUN builtin cd /opt"

    it "allows a redirected subshell" $
      ruleCatchesNot "DL3003" "RUN (cd /tmp/build && make) > /tmp/build.log"

    it "does not exempt cd in a condition outside a subshell" $
      ruleCatches "DL3003" "RUN if cd /opt; then (cd /tmp/build && make); fi"

    it "does not mistake a quoted subshell for a directory scope" $
      ruleCatches "DL3003" "RUN echo '(cd /tmp)' && { cd /opt; }"

    it "keeps cd in command substitutions checked" $
      ruleCatches "DL3003" "RUN echo $(cd /tmp && pwd)"

    it "does not change the handling of exec form" $
      ruleCatchesNot "DL3003" "RUN [\"/bin/echo\", \"cd /tmp\"]"

    it "does not skip other rules inside subshells" $
      ruleCatches "DL3004" "RUN (cd /tmp && sudo make install)"

    it "keeps an explicit subshell in ONBUILD exempt" $
      ruleCatchesNot "DL3003" "ONBUILD RUN (cd /tmp/build && make)"
