module Hadolint.Rule.DL3048Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3048 - Invalid Label Key Rule" $ do
    it "not ok with reserved namespace" $ do
      ruleCatches "DL3048" "LABEL com.docker.label=\"foo\""
      ruleCatches "DL3048" "LABEL io.docker.label=\"foo\""
      ruleCatches "DL3048" "LABEL org.dockerproject.label=\"foo\""
      onBuildRuleCatches "DL3048" "LABEL com.docker.label=\"foo\""
      onBuildRuleCatches "DL3048" "LABEL io.docker.label=\"foo\""
      onBuildRuleCatches "DL3048" "LABEL org.dockerproject.label=\"foo\""
    it "not ok with invalid character" $ do
      ruleCatches "DL3048" "LABEL invalid$character=\"foo\""
      onBuildRuleCatches "DL3048" "LABEL invalid$character=\"foo\""
    it "not ok with invalid start and end characters" $ do
      ruleCatches "DL3048" "LABEL .invalid =\"foo\""
      ruleCatches "DL3048" "LABEL -invalid =\"foo\""
      ruleCatches "DL3048" "LABEL 1invalid =\"foo\""
      onBuildRuleCatches "DL3048" "LABEL .invalid=\"foo\""
      onBuildRuleCatches "DL3048" "LABEL -invalid=\"foo\""
      onBuildRuleCatches "DL3048" "LABEL 1invalid=\"foo\""
    it "not ok with consecutive dividers" $ do
      ruleCatches "DL3048" "LABEL invalid..character=\"foo\""
      ruleCatches "DL3048" "LABEL invalid--character=\"foo\""
      onBuildRuleCatches "DL3048" "LABEL invalid..character=\"foo\""
      onBuildRuleCatches "DL3048" "LABEL invalid--character=\"foo\""
    it "ok with valid labels" $ do
      ruleCatchesNot "DL3048" "LABEL org.valid-key.label3=\"foo\""
      ruleCatchesNot "DL3048" "LABEL validlabel=\"foo\""
      onBuildRuleCatchesNot "DL3048" "LABEL org.valid-key.label3=\"foo\""
      onBuildRuleCatchesNot "DL3048" "LABEL validlabel=\"foo\""
