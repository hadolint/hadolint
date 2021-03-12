module Hadolint.Rule.DL3008 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3008"
    severity = DLWarningC
    message =
      "Pin versions in apt get install. Instead of `apt-get install <package>` use `apt-get \
      \install <package>=<version>`"
    check (Run (RunArgs args _)) = foldArguments (all versionFixed . aptGetPackages) args
    check _ = True
    versionFixed package = "=" `Text.isInfixOf` package || ("/" `Text.isInfixOf` package || ".deb" `Text.isSuffixOf` package)
{-# INLINEABLE rule #-}

aptGetPackages :: ParsedShell -> [Text.Text]
aptGetPackages args =
  [ arg
    | cmd <- Shell.presentCommands args,
      Shell.cmdHasArgs "apt-get" ["install"] cmd,
      arg <- Shell.getArgsNoFlags (dropTarget cmd),
      arg /= "install"
  ]
  where
    dropTarget = Shell.dropFlagArg ["t", "target-release"]
