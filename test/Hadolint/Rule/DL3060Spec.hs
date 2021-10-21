module Hadolint.Rule.DL3060Spec (spec) where

import Data.Default
import qualified Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3060 - `yarn cache clean` missing after `yarn install`" $ do
    it "ok with non-yarn commands" $ do
      ruleCatchesNot "DL3060" "RUN foo"
      onBuildRuleCatchesNot "DL3060" "RUN foo"
    it "not ok with no cache clean" $ do
      ruleCatches "DL3060" "RUN yarn install foo"
      onBuildRuleCatches "DL3060" "RUN yarn install foo"
    it "ok with cache clean" $ do
      ruleCatchesNot "DL3060" "RUN yarn install bar && yarn cache clean"
      onBuildRuleCatchesNot "DL3060" "RUN yarn install bar && yarn cache clean"

    it "not ok when yarn install is in last stage w/o yarn clean" $
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

    it "not ok when inheriting from stage with yarn install w/o yarn clean" $
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

    it "ok when inheriting from stage with yarn cache clear" $
      let dockerFile =
            Text.unlines
              [ "FROM node:lts-alpine as foo",
                "RUN yarn install && yarn cache clean",
                "FROM foo",
                "RUN hey!"
              ]
       in do
            ruleCatchesNot "DL3060" dockerFile
            onBuildRuleCatchesNot "DL3060" dockerFile

    it "ok when omitting yarn cache clean in stage that is not reused later" $
      let dockerFile =
            Text.unlines
              [ "FROM node:lts-alpine as foo",
                "RUN yarn install foo",
                "FROM scratch",
                "RUN hey!"
              ]
       in do
            ruleCatchesNot "DL3060" dockerFile
            onBuildRuleCatches "DL3060" dockerFile
