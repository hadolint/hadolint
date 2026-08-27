module Hadolint.Rule.DL3009 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils


rule :: Rule Shell.ParsedShell
rule = dl3009 <> onbuild dl3009
{-# INLINEABLE rule #-}

dl3009 :: Rule Shell.ParsedShell
dl3009 = simpleRule code severity message check
  where
    code = "DL3009"
    severity = DLInfoC
    message =
      "Use BuildKit cache mounts for apt (`--mount=type=cache,target=/var/cache/apt \
      \--mount=type=cache,target=/var/lib/apt`) to cache packages across builds"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands hasAptUpdate) args = True
      | Utils.hasCacheOrTmpfsMountWith "/var/lib/apt/lists" flags = True
      | Utils.hasCacheOrTmpfsMountWith "/var/lib/apt" flags
        && Utils.hasCacheOrTmpfsMountWith "/var/cache/apt" flags = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3009 #-}

hasAptUpdate :: Shell.Command -> Bool
hasAptUpdate cmd =
  Shell.cmdHasArgs "apt" ["update"] cmd
    || Shell.cmdHasArgs "apt-get" ["update"] cmd
    || Shell.cmdHasArgs "aptitude" ["update"] cmd
