module Hadolint.Rule.DL3034 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule Shell.ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3034"
    severity = DLWarningC
    message = "Non-interactive switch missing from `zypper` command: `zypper install -y`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotZypperYesOption) args
    check _ = True

    forgotZypperYesOption cmd = isZypperInstall cmd && not (hasYesOption cmd)
    isZypperInstall =
      Shell.cmdHasArgs
        "zypper"
        [ "install",
          "in",
          "remove",
          "rm",
          "source-install",
          "si",
          "patch"
        ]
    hasYesOption = Shell.hasAnyFlag ["no-confirm", "y"]
{-# INLINEABLE rule #-}
