module Hadolint.Rule.DL3026Spec (spec) where

import Data.Text as Text
import Hadolint (Configuration (..))
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = mempty
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
      let ?config = mempty { allowedRegistries = ["docker.io"] }

      ruleCatches "DL3026" $ Text.unlines dockerFile

    it "does not warn on allowed registries" $ do
      let dockerFile =
            [ "FROM random.com/debian"
            ]
      let ?config = mempty { allowedRegistries = ["x.com", "random.com"] }

      ruleCatchesNot "DL3026" $ Text.unlines dockerFile

    it "doesn't warn on scratch image" $ do
      let dockerFile =
            [ "FROM scratch"
            ]
      let ?config = mempty { allowedRegistries = ["x.com", "random.com"] }

      ruleCatchesNot "DL3026" $ Text.unlines dockerFile

    it "allows boths all forms of docker.io" $ do
      let dockerFile =
            [ "FROM ubuntu:18.04 AS builder1",
              "FROM zemanlx/ubuntu:18.04 AS builder2",
              "FROM docker.io/zemanlx/ubuntu:18.04 AS builder3"
            ]
      let ?config = mempty { allowedRegistries = ["docker.io"] }

      ruleCatchesNot "DL3026" $ Text.unlines dockerFile

    it "allows using previous stages" $ do
      let dockerFile =
            [ "FROM random.com/foo AS builder1",
              "FROM builder1 AS builder2"
            ]
      let ?config = mempty { allowedRegistries = ["random.com"] }

      ruleCatchesNot "DL3026" $ Text.unlines dockerFile
