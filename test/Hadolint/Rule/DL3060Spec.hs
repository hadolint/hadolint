module Hadolint.Rule.DL3060Spec (spec) where

import Data.Default
import qualified Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3060 - Use BuildKit cache mount for yarn." $ do
    it "don't warn: non-yarn commands" $ do
      ruleCatchesNot "DL3060" "RUN foo"
      onBuildRuleCatchesNot "DL3060" "RUN foo"

    it "warn: yarn install without cache mount" $ do
      ruleCatches "DL3060" "RUN yarn install foo"
      onBuildRuleCatches "DL3060" "RUN yarn install foo"

    it "warn: yarn cache clean no longer suppresses" $ do
      ruleCatches "DL3060" "RUN yarn install bar && yarn cache clean"
      onBuildRuleCatches "DL3060" "RUN yarn install bar && yarn cache clean"

    it "warn: yarn install regardless of order" $ do
      ruleCatches "DL3060" "RUN yarn cache clean && yarn install foo"

    it "don't warn: cache mount is used" $
      let line = "RUN --mount=type=cache,target=/root/.cache/yarn yarn install foobar"
      in do
        ruleCatchesNot "DL3060" line
        onBuildRuleCatchesNot "DL3060" line

    it "don't warn: tmpfs mount is used" $
      let line = "RUN --mount=type=tmpfs,target=/root/.cache/yarn yarn install foobar"
      in do
        ruleCatchesNot "DL3060" line
        onBuildRuleCatchesNot "DL3060" line

    it "don't warn: cache mount with non-root user home" $
      let line = "RUN --mount=type=cache,target=/home/node/.cache/yarn yarn install foobar"
      in do
        ruleCatchesNot "DL3060" line
        onBuildRuleCatchesNot "DL3060" line

    it "warn: cache mount in wrong location" $
      let line = "RUN --mount=type=cache,target=/var/lib/foobar yarn install foobar"
      in do
        ruleCatches "DL3060" line
        onBuildRuleCatches "DL3060" line

    it "warn: tmpfs mount in wrong location" $
      let line = "RUN --mount=type=tmpfs,target=/var/lib/foobar yarn install foobar"
      in do
        ruleCatches "DL3060" line
        onBuildRuleCatches "DL3060" line

    it "warn: yarn install in any stage without cache mount" $
      let dockerFile =
            Text.unlines
              [ "FROM node:lts-alpine as foo",
                "RUN hey!",
                "FROM scratch",
                "RUN yarn install"
              ]
       in do
            ruleCatches "DL3060" dockerFile
            onBuildRuleCatches "DL3060" dockerFile

    it "warn: yarn install without cache mount even in intermediate stages" $
      let dockerFile =
            Text.unlines
              [ "FROM node:lts-alpine as foo",
                "RUN yarn install",
                "FROM foo",
                "RUN hey!"
              ]
       in do
            ruleCatches "DL3060" dockerFile
            onBuildRuleCatches "DL3060" dockerFile

    it "warn: yarn cache clean no longer suppresses in inherited stage" $
      let dockerFile =
            Text.unlines
              [ "FROM node:lts-alpine as foo",
                "RUN yarn install && yarn cache clean",
                "FROM foo",
                "RUN hey!"
              ]
       in do
            ruleCatches "DL3060" dockerFile
            onBuildRuleCatches "DL3060" dockerFile

    it "warn: yarn install without cache mount in unused intermediate stage" $
      let dockerFile =
            Text.unlines
              [ "FROM node:lts-alpine as foo",
                "RUN yarn install foo",
                "FROM scratch",
                "RUN hey!"
              ]
       in do
            ruleCatches "DL3060" dockerFile
            onBuildRuleCatches "DL3060" dockerFile
