module Hadolint.Rule.DL3015 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3015"
    severity = DLInfoC
    message = "Avoid additional packages by specifying `--no-install-recommends`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotNoInstallRecommends) args
    check _ = True

    forgotNoInstallRecommends cmd = isAptGetInstall cmd && not (disablesRecommendOption cmd)
    isAptGetInstall = Shell.cmdHasArgs "apt-get" ["install"]
    disablesRecommendOption cmd =
      Shell.hasFlag "no-install-recommends" cmd
        || Shell.hasArg "APT::Install-Recommends=false" cmd
{-# INLINEABLE rule #-}
