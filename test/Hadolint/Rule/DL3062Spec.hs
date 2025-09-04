module Hadolint.Rule.DL3062Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def
  describe "DL3062 - Install only production dependencies." $ do
    it "npm install with production flag is fine" $ do
      ruleCatchesNot "DL3062" "RUN npm install --production"
      onBuildRuleCatchesNot "DL3062" "RUN npm install --production"

    it "npm ci with production flag is fine" $ do
      ruleCatchesNot "DL3062" "RUN npm ci --production"
      onBuildRuleCatchesNot "DL3062" "RUN npm ci --production"

    it "npm install with omit=dev flag is fine" $ do
      ruleCatchesNot "DL3062" "RUN npm install --omit=dev"
      onBuildRuleCatchesNot "DL3062" "RUN npm install --omit=dev"

    it "npm ci with omit=dev flag is fine" $ do
      ruleCatchesNot "DL3062" "RUN npm ci --omit=dev"
      onBuildRuleCatchesNot "DL3062" "RUN npm ci --omit=dev"

    it "npm install with production flag and other flags is fine" $ do
      ruleCatchesNot "DL3062" "RUN npm install --production --no-audit"
      onBuildRuleCatchesNot "DL3062" "RUN npm install --production --no-audit"

    it "npm install with production flag and package name is fine" $ do
      ruleCatchesNot "DL3062" "RUN npm install --production express@4.1.1"
      onBuildRuleCatchesNot "DL3062" "RUN npm install --production express@4.1.1"

    it "npm install without production flag should trigger warning" $ do
      ruleCatches "DL3062" "RUN npm install"
      onBuildRuleCatches "DL3062" "RUN npm install"

    it "npm ci without production flag should trigger warning" $ do
      ruleCatches "DL3062" "RUN npm ci"
      onBuildRuleCatches "DL3062" "RUN npm ci"

    it "npm install with package name but without production flag should trigger warning" $ do
      ruleCatches "DL3062" "RUN npm install express@4.1.1"
      onBuildRuleCatches "DL3062" "RUN npm install express@4.1.1"

    it "npm install with other flags but without production flag should trigger warning" $ do
      ruleCatches "DL3062" "RUN npm install --no-audit"
      onBuildRuleCatches "DL3062" "RUN npm install --no-audit"

    it "npm run command should not trigger warning" $ do
      ruleCatchesNot "DL3062" "RUN npm run build"
      onBuildRuleCatchesNot "DL3062" "RUN npm run build"

    -- Additional npm flags
    it "npm install with --only=production flag is fine" $ do
      ruleCatchesNot "DL3062" "RUN npm install --only=production"
      onBuildRuleCatchesNot "DL3062" "RUN npm install --only=production"

    it "npm ci with --only=production flag is fine" $ do
      ruleCatchesNot "DL3062" "RUN npm ci --only=production"
      onBuildRuleCatchesNot "DL3062" "RUN npm ci --only=production"

    -- yarn tests
    it "yarn install with --production flag is fine" $ do
      ruleCatchesNot "DL3062" "RUN yarn install --production"
      onBuildRuleCatchesNot "DL3062" "RUN yarn install --production"

    it "yarn install with --prod flag is fine" $ do
      ruleCatchesNot "DL3062" "RUN yarn install --prod"
      onBuildRuleCatchesNot "DL3062" "RUN yarn install --prod"

    it "yarn install without production flag should trigger warning" $ do
      ruleCatches "DL3062" "RUN yarn install"
      onBuildRuleCatches "DL3062" "RUN yarn install"

    it "yarn install with other flags but without production flag should trigger warning" $ do
      ruleCatches "DL3062" "RUN yarn install --frozen-lockfile"
      onBuildRuleCatches "DL3062" "RUN yarn install --frozen-lockfile"

    it "yarn run command should not trigger warning" $ do
      ruleCatchesNot "DL3062" "RUN yarn run build"
      onBuildRuleCatchesNot "DL3062" "RUN yarn run build"

    -- pnpm tests
    it "pnpm install with --production flag is fine" $ do
      ruleCatchesNot "DL3062" "RUN pnpm install --production"
      onBuildRuleCatchesNot "DL3062" "RUN pnpm install --production"

    it "pnpm install with --prod flag is fine" $ do
      ruleCatchesNot "DL3062" "RUN pnpm install --prod"
      onBuildRuleCatchesNot "DL3062" "RUN pnpm install --prod"

    it "pnpm install with --only=production flag is fine" $ do
      ruleCatchesNot "DL3062" "RUN pnpm install --only=production"
      onBuildRuleCatchesNot "DL3062" "RUN pnpm install --only=production"

    it "pnpm ci with --production flag is fine" $ do
      ruleCatchesNot "DL3062" "RUN pnpm ci --production"
      onBuildRuleCatchesNot "DL3062" "RUN pnpm ci --production"

    it "pnpm install without production flag should trigger warning" $ do
      ruleCatches "DL3062" "RUN pnpm install"
      onBuildRuleCatches "DL3062" "RUN pnpm install"

    it "pnpm ci without production flag should trigger warning" $ do
      ruleCatches "DL3062" "RUN pnpm ci"
      onBuildRuleCatches "DL3062" "RUN pnpm ci"

    it "pnpm run command should not trigger warning" $ do
      ruleCatchesNot "DL3062" "RUN pnpm run build"
      onBuildRuleCatchesNot "DL3062" "RUN pnpm run build"
