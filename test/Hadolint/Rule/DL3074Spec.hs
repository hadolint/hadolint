module Hadolint.Rule.DL3074Spec (spec) where

import Data.Default
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3074 - Use BuildKit cache mount for NuGet/.NET" $ do
    it "warn: dotnet restore without cache mount" $ do
      ruleCatches "DL3074" "RUN dotnet restore"
      onBuildRuleCatches "DL3074" "RUN dotnet restore"

    it "warn: dotnet build without cache mount" $ do
      ruleCatches "DL3074" "RUN dotnet build"
      onBuildRuleCatches "DL3074" "RUN dotnet build"

    it "warn: nuget restore without cache mount" $ do
      ruleCatches "DL3074" "RUN nuget restore"
      onBuildRuleCatches "DL3074" "RUN nuget restore"

    it "don't warn: non-dotnet/nuget command" $ do
      ruleCatchesNot "DL3074" "RUN echo hello"
      onBuildRuleCatchesNot "DL3074" "RUN echo hello"

    it "don't warn: cache mount at /root/.nuget/packages" $ do
      ruleCatchesNot "DL3074" "RUN --mount=type=cache,target=/root/.nuget/packages dotnet restore"
      onBuildRuleCatchesNot "DL3074" "RUN --mount=type=cache,target=/root/.nuget/packages dotnet restore"

    it "don't warn: tmpfs mount at /root/.nuget/packages" $ do
      ruleCatchesNot "DL3074" "RUN --mount=type=tmpfs,target=/root/.nuget/packages dotnet restore"
      onBuildRuleCatchesNot "DL3074" "RUN --mount=type=tmpfs,target=/root/.nuget/packages dotnet restore"

    it "warn: cache mount at wrong path" $ do
      ruleCatches "DL3074" "RUN --mount=type=cache,target=/wrong/path dotnet restore"
      onBuildRuleCatches "DL3074" "RUN --mount=type=cache,target=/wrong/path dotnet restore"

    it "don't warn: non-root user home path in mount (substring match)" $ do
      ruleCatchesNot "DL3074" "RUN --mount=type=cache,target=/home/app/.nuget/packages dotnet restore"
      onBuildRuleCatchesNot "DL3074" "RUN --mount=type=cache,target=/home/app/.nuget/packages dotnet restore"
