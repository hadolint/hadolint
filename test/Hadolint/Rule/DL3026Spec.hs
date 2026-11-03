module Hadolint.Rule.DL3026Spec (spec) where

import Data.Default
import Data.Text as Text
import Hadolint (Configuration (..))
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3026 - Use only an allowed registry in the FROM image" $ do
    it "does not warn on empty allowed registries" $ do
      let dockerFile =
            [ "FROM random.com/debian"
            ]
      ruleCatchesNot "DL3026" $ Text.unlines dockerFile

    it "warn on non-allowed registry" $ do
      let dockerFile =
            [ "FROM random.com/debian"
            ]
      let ?config = def { allowedRegistries = ["docker.io"] }

      ruleCatches "DL3026" $ Text.unlines dockerFile

    it "does not warn on allowed registries" $ do
      let dockerFile =
            [ "FROM random.com/debian"
            ]
      let ?config = def { allowedRegistries = ["x.com", "random.com"] }

      ruleCatchesNot "DL3026" $ Text.unlines dockerFile

    it "doesn't warn on scratch image" $ do
      let dockerFile =
            [ "FROM scratch"
            ]
      let ?config = def { allowedRegistries = ["x.com", "random.com"] }

      ruleCatchesNot "DL3026" $ Text.unlines dockerFile

    it "allows all forms of docker.io" $ do
      let dockerFile =
            [ "FROM ubuntu:18.04 AS builder1",
              "FROM zemanlx/ubuntu:18.04 AS builder2",
              "FROM docker.io/zemanlx/ubuntu:18.04 AS builder3"
            ]
      let ?config = def { allowedRegistries = ["docker.io"] }

      ruleCatchesNot "DL3026" $ Text.unlines dockerFile

    it "allows using previous stages" $ do
      let dockerFile =
            [ "FROM random.com/foo AS builder1",
              "FROM builder1 AS builder2"
            ]
      let ?config = def { allowedRegistries = ["random.com"] }

      ruleCatchesNot "DL3026" $ Text.unlines dockerFile

    it "warn on non-allowed wildcard registry" $ do
      let dockerFile =
            [ "FROM x.com/debian"
            ]
      let ?config = def { allowedRegistries = ["*.random.com"] }

      ruleCatches "DL3026" $ Text.unlines dockerFile

    it "does not warn on allowed wildcard registries" $ do
      let dockerFile =
            [ "FROM foo.random.com/debian"
            ]
      let ?config = def { allowedRegistries = ["x.com", "*.random.com"] }

      ruleCatchesNot "DL3026" $ Text.unlines dockerFile

    it "does not warn on * registry" $ do
      let dockerFile =
            [ "FROM ubuntu:18.04 AS builder1",
              "FROM zemanlx/ubuntu:18.04 AS builder2",
              "FROM docker.io/zemanlx/ubuntu:18.04 AS builder3"
            ]
      let ?config = def { allowedRegistries = ["*"] }

      ruleCatchesNot "DL3026" $ Text.unlines dockerFile

    it "does warn on copy from untrusted registry" $ do
      let dockerfile =
            Text.unlines
              [ "COPY --from=untrusted.com/repo/image:tag /foo /bar" ]
      let ?config = def { allowedRegistries = [ "trusted.com" ] }
       in ruleCatches "DL3026" dockerfile

    it "does not warn on copy from untrusted registry" $ do
      let dockerfile =
            Text.unlines
              [ "COPY --from=trusted.com/repo/image:tag /foo /bar" ]
      let ?config = def { allowedRegistries = [ "trusted.com" ] }
       in ruleCatchesNot "DL3026" dockerfile

    it "warn on run with bind mount from untrusted registry" $ do
      let dockerfile =
            Text.unlines
              [ "RUN --mount=type=bind,from=untrusted.com/repo/image:tag,target=/foo foobar" ]
      let ?config = def { allowedRegistries = [ "trusted.com" ] }
       in ruleCatches "DL3026" dockerfile

    it "warn on run with cache mount from untrusted registry" $ do
      let dockerfile =
            Text.unlines
              [ "RUN --mount=type=cache,from=untrusted.com/repo/image:tag,target=/foo foobar" ]
      let ?config = def { allowedRegistries = [ "trusted.com" ] }
       in ruleCatches "DL3026" dockerfile

    it "don't warn on run with bind mount from trusted registry" $ do
      let dockerfile =
            Text.unlines
              [ "RUN --mount=type=bind,from=trusted.com/repo/image:tag,target=/foo foobar" ]
      let ?config = def { allowedRegistries = [ "trusted.com" ] }
       in ruleCatchesNot "DL3026" dockerfile

    it "don't warn on run with cache mount from trusted registry" $ do
      let dockerfile =
            Text.unlines
              [ "RUN --mount=type=cache,from=trusted.com/repo/image:tag,target=/foo foobar" ]
      let ?config = def { allowedRegistries = [ "trusted.com" ] }
       in ruleCatchesNot "DL3026" dockerfile

    it "distrust all forms of docker.io if trusted registries are given" $ do
      let dockerFile =
            [ "FROM ubuntu:18.04 AS builder1",
              "FROM zemanlx/ubuntu:18.04 AS builder2",
              "FROM docker.io/zemanlx/ubuntu:18.04 AS builder3"
            ]
      let ?config = def { allowedRegistries = ["trusted.com"] }
       in assertChecks ( Text.unlines dockerFile ) ( failsWith 3 "DL3026" )
