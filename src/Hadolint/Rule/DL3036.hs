module Hadolint.Rule.DL3036 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3036 <> onbuild dl3036
{-# INLINEABLE rule #-}

dl3036 :: Rule Shell.ParsedShell
dl3036 = simpleRule code severity message check
  where
    code = "DL3036"
    severity = DLWarningC
    message =
      "Use BuildKit cache mount for zypper (`--mount=type=cache,target=/var/cache/zypp`) \
      \or run `zypper clean` after zypper command"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands zypperInstall) args = True
      | Utils.hasCacheOrTmpfsMountWith "/var/cache/zypp" flags = True
      | otherwise = False
    check _ = True

    zypperInstall = Shell.cmdHasArgs "zypper" ["install", "in"]
{-# INLINEABLE dl3036 #-}
