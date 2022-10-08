module Hadolint.Rule.DL3040 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
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

    check (Run (RunArgs args _)) = all (checkMissingClean args) dnfCmds
    check _ = True

    checkMissingClean args cmdName =
      foldArguments (Shell.noCommands $ dnfInstall cmdName) args
        || ( foldArguments (Shell.anyCommands $ dnfInstall cmdName) args
               && foldArguments (Shell.anyCommands $ dnfClean cmdName) args
           )

    dnfInstall cmdName = Shell.cmdHasArgs cmdName ["install"]
    dnfClean cmdName args = Shell.cmdHasArgs cmdName ["clean", "all"] args 
      || Shell.cmdHasArgs "rm" ["-rf", "/var/cache/yum/*"] args
    dnfCmds = ["dnf", "microdnf"]
{-# INLINEABLE dl3040 #-}
