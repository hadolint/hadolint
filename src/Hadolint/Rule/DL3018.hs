module Hadolint.Rule.DL3018 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3018"
    severity = DLWarningC
    message =
      "Pin versions in apk add. Instead of `apk add <package>` use `apk add <package>=<version>`"
    check (Run (RunArgs args _)) = foldArguments (\as -> and [versionFixed p | p <- apkAddPackages as]) args
    check _ = True
    versionFixed package = "=" `Text.isInfixOf` package
{-# INLINEABLE rule #-}

apkAddPackages :: ParsedShell -> [Text.Text]
apkAddPackages args =
  [ arg
    | cmd <- Shell.presentCommands args,
      Shell.cmdHasArgs "apk" ["add"] cmd,
      arg <- Shell.getArgsNoFlags (dropTarget cmd),
      arg /= "add"
  ]
  where
    dropTarget = Shell.dropFlagArg ["t", "virtual", "repository", "X"]
