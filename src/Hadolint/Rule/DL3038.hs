module Hadolint.Rule.DL3038 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3038 <> onbuild dl3038
{-# INLINEABLE rule #-}

dl3038 :: Rule Shell.ParsedShell
dl3038 = simpleRule code severity message check
  where
    code = "DL3038"
    severity = DLWarningC
    message = "Use the -y switch to avoid manual input `dnf install -y <package>`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotDnfYesOption) args
    check _ = True

    forgotDnfYesOption cmd = isDnfInstall cmd && not (hasYesOption cmd)
    isDnfInstall = Shell.cmdsHaveArgs dnfCmds ["install", "groupinstall", "localinstall"]
    hasYesOption = Shell.hasAnyFlag ["y", "assumeyes"]
    dnfCmds = ["dnf", "microdnf"]
{-# INLINEABLE dl3038 #-}
