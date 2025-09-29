module Hadolint.Rule.DL3062Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec

spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3062 - Pin versions in go install." $ do
    describe "go install" $ do
      it "go version not pinned" $ do
        ruleCatches "DL3062" "RUN go install example.com/pkg"
        onBuildRuleCatches "DL3062" "RUN go install example.com/pkg"
      it "go version pinned as latest" $ do
        ruleCatches "DL3062" "RUN go install example.com/pkg@latest"
        onBuildRuleCatches "DL3062" "RUN go install example.com/pkg@latest"
      it "go version pinned" $ do
        ruleCatchesNot "DL3062" "RUN go install example.com/pkg@v1.2.3"
        onBuildRuleCatchesNot "DL3062" "RUN go install example.com/pkg@v1.2.3"

      it "go install with local absolute path" $ do
        ruleCatchesNot "DL3062" "RUN go install /go/app/foobar"
        onBuildRuleCatchesNot "DL3062" "RUN go install /go/app/foobar"
      it "go install with local relative path" $ do
        ruleCatchesNot "DL3062" "RUN go install ./app/foobar"
        onBuildRuleCatchesNot "DL3062" "RUN go install ./app/foobar"
      it "go install local dir" $ do
        ruleCatchesNot "DL3062" "RUN go install ."
        onBuildRuleCatchesNot "DL3062" "RUN go install ."

      describe "go install tool" $ do
        it "version not pinned" $ do
          ruleCatches "DL3062" "RUN go install tool foobar"
          onBuildRuleCatches "DL3062" "RUN go install tool foobar"
        it "version pinned as latest" $ do
          ruleCatches "DL3062" "RUN go install tool foobar@latest"
          onBuildRuleCatches "DL3062" "RUN go install tool foobar@latest"
        it "version pinned" $ do
          ruleCatchesNot "DL3062" "RUN go install tool foobar@v1.2.3"
          onBuildRuleCatchesNot "DL3062" "RUN go install tool foobar@v1.2.3"

    describe "go get" $ do
      it "go version not pinned" $ do
        ruleCatches "DL3062" "RUN go get example.com/pkg"
        onBuildRuleCatches "DL3062" "RUN go get example.com/pkg"
      it "go version pinned as latest" $ do
        ruleCatches "DL3062" "RUN go get example.com/pkg@latest"
        onBuildRuleCatches "DL3062" "RUN go get example.com/pkg@latest"
      it "go version pinned" $ do
        ruleCatchesNot "DL3062" "RUN go get example.com/pkg@v1.2.3"
        onBuildRuleCatchesNot "DL3062" "RUN go get example.com/pkg@v1.2.3"

      describe "go get tool" $ do
        it "version not pinned" $ do
          ruleCatches "DL3062" "RUN go get -tool foobar"
          onBuildRuleCatches "DL3062" "RUN go get -tool foobar"
        it "version pinned as latest" $ do
          ruleCatches "DL3062" "RUN go get -tool foobar@latest"
          onBuildRuleCatches "DL3062" "RUN go get -tool foobar@latest"
        it "version pinned" $ do
          ruleCatchesNot "DL3062" "RUN go get -tool foobar@v1.2.3"
          onBuildRuleCatchesNot "DL3062" "RUN go get -tool foobar@v1.2.3"

    describe "go run" $ do
      it "go version not pinned" $ do
        ruleCatches "DL3062" "RUN go run example.com/pkg"
        onBuildRuleCatches "DL3062" "RUN go run example.com/pkg"
      it "go version pinned as latest" $ do
        ruleCatches "DL3062" "RUN go run example.com/pkg@latest"
        onBuildRuleCatches "DL3062" "RUN go run example.com/pkg@latest"
      it "go version pinned" $ do
        ruleCatchesNot "DL3062" "RUN go run example.com/pkg@v1.2.3"
        onBuildRuleCatchesNot "DL3062" "RUN go run example.com/pkg@v1.2.3"

      it "go version pinned with flags" $ do
        ruleCatchesNot "DL3062" "RUN go run example.com/pkg@v1.2.3 -f"
        onBuildRuleCatchesNot "DL3062" "RUN go run example.com/pkg@v1.2.3 -f"
        ruleCatchesNot "DL3062" "RUN go run example.com/pkg@v1.2.3 -foobar"
        onBuildRuleCatchesNot "DL3062" "RUN go run example.com/pkg@v1.2.3 -foobar"
        ruleCatchesNot "DL3062" "RUN go run example.com/pkg@v1.2.3 --foobar"
        onBuildRuleCatchesNot "DL3062" "RUN go run example.com/pkg@v1.2.3 --foobar"
      it "go version pinned with arguments" $ do
        ruleCatchesNot "DL3062" "RUN go run example.com/pkg@v1.2.3 foobar"
        onBuildRuleCatchesNot "DL3062" "RUN go run example.com/pkg@v1.2.3 foobar"
        ruleCatchesNot "DL3062" "RUN go run example.com/pkg@v1.2.3 --foo bar"
        onBuildRuleCatchesNot "DL3062" "RUN go run example.com/pkg@v1.2.3 --foo bar"

      it "go run with local absolute path" $ do
        ruleCatchesNot "DL3062" "RUN go run /go/app/foobar"
        onBuildRuleCatchesNot "DL3062" "RUN go run /go/app/foobar"
      it "go run with local relative path" $ do
        ruleCatchesNot "DL3062" "RUN go run ./app/foobar"
        onBuildRuleCatchesNot "DL3062" "RUN go run ./app/foobar"
      it "go run local dir" $ do
        ruleCatchesNot "DL3062" "RUN go run ."
        onBuildRuleCatchesNot "DL3062" "RUN go run ."
