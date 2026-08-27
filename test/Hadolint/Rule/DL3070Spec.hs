module Hadolint.Rule.DL3070Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3070 - Use BuildKit cache mount for Go" $ do
    it "warn: go build without cache mount" $ do
      ruleCatches "DL3070" "RUN go build ./..."
      onBuildRuleCatches "DL3070" "RUN go build ./..."

    it "warn: go get without cache mount" $ do
      ruleCatches "DL3070" "RUN go get github.com/foo/bar"
      onBuildRuleCatches "DL3070" "RUN go get github.com/foo/bar"

    it "warn: go mod without cache mount" $ do
      ruleCatches "DL3070" "RUN go mod download"
      onBuildRuleCatches "DL3070" "RUN go mod download"

    it "don't warn: non-go command" $ do
      ruleCatchesNot "DL3070" "RUN echo hello"
      onBuildRuleCatchesNot "DL3070" "RUN echo hello"

    it "don't warn: cache mount at /root/.cache/go-build" $ do
      ruleCatchesNot "DL3070" "RUN --mount=type=cache,target=/root/.cache/go-build go build ./..."
      onBuildRuleCatchesNot "DL3070" "RUN --mount=type=cache,target=/root/.cache/go-build go build ./..."

    it "don't warn: cache mount at /go/pkg/mod" $ do
      ruleCatchesNot "DL3070" "RUN --mount=type=cache,target=/go/pkg/mod go build ./..."
      onBuildRuleCatchesNot "DL3070" "RUN --mount=type=cache,target=/go/pkg/mod go build ./..."

    it "don't warn: tmpfs mount at /root/.cache/go-build" $ do
      ruleCatchesNot "DL3070" "RUN --mount=type=tmpfs,target=/root/.cache/go-build go build ./..."
      onBuildRuleCatchesNot "DL3070" "RUN --mount=type=tmpfs,target=/root/.cache/go-build go build ./..."

    it "warn: cache mount at wrong path" $ do
      ruleCatches "DL3070" "RUN --mount=type=cache,target=/wrong/path go build ./..."
      onBuildRuleCatches "DL3070" "RUN --mount=type=cache,target=/wrong/path go build ./..."
