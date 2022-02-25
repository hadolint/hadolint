module Hadolint.Rule.DL3040 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule Shell.ParsedShell
rule = simpleRule code severity message check
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
    dnfClean cmdName = Shell.cmdHasArgs cmdName ["clean", "all"]
    dnfCmds = ["dnf", "microdnf"]
{-# INLINEABLE rule #-}
