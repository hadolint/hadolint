module Hadolint.Rule.DL4005 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL4005"
    severity = DLWarningC
    message = "Use SHELL to change the default shell"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands (Shell.cmdHasArgs "ln" ["/bin/sh"])) args
    check _ = True
{-# INLINEABLE rule #-}
