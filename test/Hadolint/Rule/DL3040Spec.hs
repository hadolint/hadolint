module Hadolint.Rule.DL3040Spec (spec) where

import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = mempty
  describe "DL3040 - `dnf clean all` missing after dnf command." $ do
    it "no ok without dnf clean all" $ do
      ruleCatches "DL3040" "RUN dnf install -y mariadb-10.4"
      onBuildRuleCatches "DL3040" "RUN dnf install -y mariadb-10.4"
    it "ok with dnf clean all" $ do
      ruleCatchesNot "DL3040" "RUN dnf install -y mariadb-10.4 && dnf clean all"
      ruleCatchesNot "DL3040" "RUN notdnf install mariadb"
      onBuildRuleCatchesNot "DL3040" "RUN dnf install -y mariadb-10.4 && dnf clean all"
      onBuildRuleCatchesNot "DL3040" "RUN notdnf install mariadb"
