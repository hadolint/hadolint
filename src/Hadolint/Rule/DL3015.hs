module Hadolint.Rule.DL3015 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax


rule :: Rule ParsedShell
rule = dl3015 <> onbuild dl3015
{-# INLINEABLE rule #-}

dl3015 :: Rule ParsedShell
dl3015 = simpleRule code severity message check
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
{-# INLINEABLE dl3015 #-}
