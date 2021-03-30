module DL3026 (tests) where

import Data.Map as Map
import Data.Text as Text
import Hadolint.Process
import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL2036 - Use only an allowed registry in the FROM image" $ do
    it "does not warn on empty allowed registries" $ do
      let dockerFile =
            [ "FROM random.com/debian"
            ]
      ruleCatchesNot "DL3026" $ Text.unlines dockerFile

    it "warn on non-allowed registry" $ do
      let dockerFile =
            [ "FROM random.com/debian"
            ]
      let ?rulesConfig = Hadolint.Process.RulesConfig ["docker.io"] Map.empty False
      ruleCatches "DL3026" $ Text.unlines dockerFile

    it "does not warn on allowed registries" $ do
      let dockerFile =
            [ "FROM random.com/debian"
            ]
      let ?rulesConfig = Hadolint.Process.RulesConfig ["x.com", "random.com"] Map.empty False
      ruleCatchesNot "DL3026" $ Text.unlines dockerFile

    it "doesn't warn on scratch image" $ do
      let dockerFile =
            [ "FROM scratch"
            ]
      let ?rulesConfig = Hadolint.Process.RulesConfig ["x.com", "random.com"] Map.empty False
      ruleCatchesNot "DL3026" $ Text.unlines dockerFile

    it "allows boths all forms of docker.io" $ do
      let dockerFile =
            [ "FROM ubuntu:18.04 AS builder1",
              "FROM zemanlx/ubuntu:18.04 AS builder2",
              "FROM docker.io/zemanlx/ubuntu:18.04 AS builder3"
            ]
      let ?rulesConfig = Hadolint.Process.RulesConfig ["docker.io"] Map.empty False
      ruleCatchesNot "DL3026" $ Text.unlines dockerFile

    it "allows using previous stages" $ do
      let dockerFile =
            [ "FROM random.com/foo AS builder1",
              "FROM builder1 AS builder2"
            ]
      let ?rulesConfig = Hadolint.Process.RulesConfig ["random.com"] Map.empty False
      ruleCatchesNot "DL3026" $ Text.unlines dockerFile
