module Hadolint.Rule.DL3028 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax (Instruction (..), RunArgs (..))

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3028"
    severity = DLWarningC
    message =
      "Pin versions in gem install. Instead of `gem install <gem>` use `gem \
      \install <gem>:<version>`"

    check (Run (RunArgs args _)) = foldArguments (all versionFixed . gems) args
    check _ = True

    versionFixed package = ":" `Text.isInfixOf` package
{-# INLINEABLE rule #-}

gems :: Shell.ParsedShell -> [Text.Text]
gems shell =
  [ arg
    | cmd <- Shell.presentCommands shell,
      Shell.cmdHasArgs "gem" ["install", "i"] cmd,
      not (Shell.cmdHasArgs "gem" ["-v"] cmd),
      not (Shell.cmdHasArgs "gem" ["--version"] cmd),
      not (Shell.cmdHasPrefixArg "gem" "--version=" cmd),
      arg <- Shell.getArgsNoFlags cmd,
      arg /= "install",
      arg /= "i",
      arg /= "--"
  ]
