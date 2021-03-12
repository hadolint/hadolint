module Hadolint.Rule.DL3030 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule Shell.ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3030"
    severity = DLWarningC
    message = "Use the -y switch to avoid manual input `yum install -y <package`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotYumYesOption) args
    check _ = True

    forgotYumYesOption cmd = isYumInstall cmd && not (hasYesOption cmd)
    isYumInstall = Shell.cmdHasArgs "yum" ["install", "groupinstall", "localinstall"]
    hasYesOption = Shell.hasAnyFlag ["y", "assumeyes"]
{-# INLINEABLE rule #-}
