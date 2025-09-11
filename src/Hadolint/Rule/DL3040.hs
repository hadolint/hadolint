module Hadolint.Rule.DL3040 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3040 <> onbuild dl3040
{-# INLINEABLE rule #-}

dl3040 :: Rule Shell.ParsedShell
dl3040 = simpleRule code severity message check
  where
    code = "DL3040"
    severity = DLWarningC
    message = "`dnf clean all` missing after dnf command."

    check (Run (RunArgs args flags))
      | all (checkMissingClean args) dnfCmds = True
      | Utils.hasCacheOrTmpfsMountWith "/var/cache/libdnf5" flags = True
      | Utils.hasCacheOrTmpfsMountWith ".cache/libdnf5" flags = True
      | otherwise = False
    check _ = True

    checkMissingClean args cmdName =
      foldArguments (Shell.noCommands $ dnfInstall cmdName) args
        || ( foldArguments (Shell.anyCommands $ dnfInstall cmdName) args
               && foldArguments (Shell.anyCommands $ dnfClean cmdName) args
           )

    dnfInstall cmdName = Shell.cmdHasArgs cmdName ["install"]
    dnfClean cmdName args = Shell.cmdHasArgs cmdName ["clean", "all"] args
      || Shell.cmdHasArgs "rm" ["-rf", "/var/cache/libdnf5*"] args
    dnfCmds = ["dnf", "microdnf"]
{-# INLINEABLE dl3040 #-}
