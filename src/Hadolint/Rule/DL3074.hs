module Hadolint.Rule.DL3074 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3074 <> onbuild dl3074
{-# INLINEABLE rule #-}

dl3074 :: Rule Shell.ParsedShell
dl3074 = simpleRule code severity message check
  where
    code = "DL3074"
    severity = DLInfoC
    message =
      "Use BuildKit cache mount for NuGet (`--mount=type=cache,target=/root/.nuget/packages`) \
      \to speed up .NET dependency restoration"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands isDotnetOrNugetCommand) args = True
      | Utils.hasCacheOrTmpfsMountWith ".nuget/packages" flags = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3074 #-}

isDotnetOrNugetCommand :: Shell.Command -> Bool
isDotnetOrNugetCommand cmd =
  Shell.cmdHasArgs "dotnet" ["restore", "build", "publish", "test"] cmd
    || Shell.cmdHasArgs "nuget" ["restore"] cmd
