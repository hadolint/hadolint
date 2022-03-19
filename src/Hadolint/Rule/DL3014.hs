module Hadolint.Rule.DL3014 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax


rule :: Rule ParsedShell
rule = dl3014 <> onbuild dl3014
{-# INLINEABLE rule #-}

dl3014 :: Rule ParsedShell
dl3014 = simpleRule code severity message check
  where
    code = "DL3014"
    severity = DLWarningC
    message = "Use the `-y` switch to avoid manual input `apt-get -y install <package>`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotAptYesOption) args
    check _ = True

    forgotAptYesOption cmd = isAptGetInstall cmd && not (hasYesOption cmd)
    isAptGetInstall = Shell.cmdHasArgs "apt-get" ["install"]
    hasYesOption = Shell.hasAnyFlag ["y", "yes", "q", "assume-yes"]
{-# INLINEABLE dl3014 #-}
