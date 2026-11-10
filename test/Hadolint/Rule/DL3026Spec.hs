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

    it "allows an image in a trusted namespace" $ do
      let ?config = def { allowedRegistries = ["gcr.io/distroless"] }
       in ruleCatchesNot "DL3026" "FROM gcr.io/distroless/base:stable"

    it "rejects a sibling namespace" $ do
      let ?config = def { allowedRegistries = ["gcr.io/distroless"] }
       in ruleCatches "DL3026" "FROM gcr.io/other/base:stable"

    it "does not treat a namespace as a partial path prefix" $ do
      let ?config = def { allowedRegistries = ["gcr.io/distroless"] }
       in ruleCatches "DL3026" "FROM gcr.io/distroless-evil/base:stable"

    it "allows nested namespace paths" $ do
      let ?config = def { allowedRegistries = ["gcr.io/team/approved"] }
       in ruleCatchesNot "DL3026" "FROM gcr.io/team/approved/base:stable"

    it "rejects a different nested namespace" $ do
      let ?config = def { allowedRegistries = ["gcr.io/team/approved"] }
       in ruleCatches "DL3026" "FROM gcr.io/team/other/base:stable"

    it "allows a trailing namespace wildcard" $ do
      let ?config = def { allowedRegistries = ["gcr.io/distroless/*"] }
       in ruleCatchesNot "DL3026" "FROM gcr.io/distroless/base:stable"

    it "keeps wildcard namespace boundaries" $ do
      let ?config = def { allowedRegistries = ["gcr.io/distroless/*"] }
       in ruleCatches "DL3026" "FROM gcr.io/distroless-evil/base:stable"

    it "keeps host-wide trust" $ do
      let ?config = def { allowedRegistries = ["gcr.io"] }
       in ruleCatchesNot "DL3026" "FROM gcr.io/other/base:stable"

    it "allows a namespace with a registry port" $ do
      let ?config = def { allowedRegistries = ["registry.example.com:5000/team"] }
       in ruleCatchesNot "DL3026" "FROM registry.example.com:5000/team/base:stable"

    it "allows an implicit Docker Hub namespace" $ do
      let ?config = def { allowedRegistries = ["docker.io/team"] }
       in ruleCatchesNot "DL3026" "FROM team/base:stable"

    it "allows the legacy Docker Hub hostname with a namespace" $ do
      let ?config = def { allowedRegistries = ["hub.docker.com/team"] }
       in ruleCatchesNot "DL3026" "FROM team/base:stable"

    it "allows a trusted namespace in COPY" $ do
      let ?config = def { allowedRegistries = ["gcr.io/distroless"] }
       in ruleCatchesNot "DL3026" "COPY --from=gcr.io/distroless/base:stable /foo /bar"

    it "rejects a sibling namespace in COPY" $ do
      let ?config = def { allowedRegistries = ["gcr.io/distroless"] }
       in ruleCatches "DL3026" "COPY --from=gcr.io/other/base:stable /foo /bar"

    it "allows a trusted namespace in a bind mount" $ do
      let ?config = def { allowedRegistries = ["gcr.io/distroless"] }
       in ruleCatchesNot "DL3026" "RUN --mount=type=bind,from=gcr.io/distroless/base:stable,target=/foo foobar"

    it "rejects a sibling namespace in a cache mount" $ do
      let ?config = def { allowedRegistries = ["gcr.io/distroless"] }
       in ruleCatches "DL3026" "RUN --mount=type=cache,from=gcr.io/other/base:stable,target=/foo foobar"

    it "allows a trusted namespace in a cache mount" $ do
      let ?config = def { allowedRegistries = ["gcr.io/distroless"] }
       in ruleCatchesNot "DL3026" "RUN --mount=type=cache,from=gcr.io/distroless/base:stable,target=/foo foobar"

    it "allows namespace trust with wildcard hosts" $ do
      let ?config = def { allowedRegistries = ["*.example.com/team"] }
       in ruleCatchesNot "DL3026" "FROM registry.example.com/team/base:stable"

    it "does not match host suffixes against image paths" $ do
      let ?config = def { allowedRegistries = ["*.example.com"] }
       in ruleCatches "DL3026" "FROM evil.com/team.example.com:stable"

    it "allows an explicitly trusted tagged image in COPY" $ do
      let ?config = def { allowedRegistries = ["gcr.io/team/base"] }
       in ruleCatchesNot "DL3026" "COPY --from=gcr.io/team/base:stable /foo /bar"

    it "allows an explicitly trusted digest image in COPY" $ do
      let ?config = def { allowedRegistries = ["gcr.io/team/base"] }
       in ruleCatchesNot "DL3026" "COPY --from=gcr.io/team/base@sha256:abc /foo /bar"

    it "rejects a near-match for an explicitly trusted image" $ do
      let ?config = def { allowedRegistries = ["gcr.io/team/base"] }
       in ruleCatches "DL3026" "COPY --from=gcr.io/team/base-evil:stable /foo /bar"

    it "rejects another host for a trusted namespace" $ do
      let ?config = def { allowedRegistries = ["gcr.io/team"] }
       in ruleCatches "DL3026" "FROM evil.com/team/base:stable"
