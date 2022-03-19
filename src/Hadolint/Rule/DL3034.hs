module Hadolint.Rule.DL3034 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3034 <> onbuild dl3034
{-# INLINEABLE rule #-}

dl3034 :: Rule Shell.ParsedShell
dl3034 = simpleRule code severity message check
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
    hasYesOption = Shell.hasAnyFlag ["non-interactive", "n", "no-confirm", "y"]
{-# INLINEABLE dl3034 #-}
