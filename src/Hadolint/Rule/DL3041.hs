module Hadolint.Rule.DL3041 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule Shell.ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3041"
    severity = DLWarningC
    message = "Specify version with `dnf install -y <package>-<version>`."

    check (Run (RunArgs args _)) = foldArguments (all versionFixed . dnfPackages) args
    check _ = True

    versionFixed package =
      "-" `Text.isInfixOf` package
        || ".rpm" `Text.isSuffixOf` package
{-# INLINEABLE rule #-}

dnfPackages :: Shell.ParsedShell -> [Text.Text]
dnfPackages args =
  [ arg | cmd <- Shell.presentCommands args, Shell.cmdHasArgs "dnf" ["install"] cmd, arg <- Shell.getArgsNoFlags cmd, arg /= "install"
  ]
