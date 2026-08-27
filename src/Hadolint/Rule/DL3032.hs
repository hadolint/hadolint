module Hadolint.Rule.DL3032 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3032 <> onbuild dl3032
{-# INLINEABLE rule #-}

dl3032 :: Rule Shell.ParsedShell
dl3032 = simpleRule code severity message check
  where
    code = "DL3032"
    severity = DLWarningC
    message =
      "Use BuildKit cache mount for yum (`--mount=type=cache,target=/var/cache/yum`) \
      \or run `yum clean all` after yum command"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands yumInstall) args = True
      | Utils.hasCacheOrTmpfsMountWith "/var/cache/yum" flags = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3032 #-}

yumInstall :: Shell.Command -> Bool
yumInstall = Shell.cmdHasArgs "yum" ["install"]
