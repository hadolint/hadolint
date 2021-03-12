module Hadolint.Rule.DL3037 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule Shell.ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3037"
    severity = DLWarningC
    message = "Specify version with `zypper install -y <package>=<version>`."

    check (Run (RunArgs args _)) = foldArguments (all versionFixed . zypperPackages) args
    check _ = True

    versionFixed package =
      "=" `Text.isInfixOf` package
        || ">=" `Text.isInfixOf` package
        || ">" `Text.isInfixOf` package
        || "<=" `Text.isInfixOf` package
        || "<" `Text.isInfixOf` package
        || ".rpm" `Text.isSuffixOf` package
{-# INLINEABLE rule #-}

zypperPackages :: Shell.ParsedShell -> [Text.Text]
zypperPackages args =
  [ arg | cmd <- Shell.presentCommands args, Shell.cmdHasArgs "zypper" ["install", "in"] cmd, arg <- Shell.getArgsNoFlags cmd, arg /= "install", arg /= "in"
  ]
