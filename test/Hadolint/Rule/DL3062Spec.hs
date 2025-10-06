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
