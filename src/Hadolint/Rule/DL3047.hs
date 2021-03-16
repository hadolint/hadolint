module Hadolint.Rule.DL3047 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3047"
    severity = DLWarningC
    message =
      "Avoid use of wget without progress bar. Use `wget --progress=dot:giga <url>`.\
      \Or consider using `-q` or `-nv` (shorthands for `--quiet` or `--no-verbose`)."

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotProgress) args
    check _ = True

    forgotProgress cmd = isWget cmd && (not (hasProgressOption cmd) && not (hasSpecialFlags cmd))
    isWget (Shell.Command name _ _) = name == "wget"
    hasProgressOption cmd = Shell.hasFlag "progress" cmd

    hasSpecialFlags cmd =
      hasQuietFlag cmd
        || hasOutputFlag cmd
        || hasAppendOutputFlag cmd
        || hasNoVerboseFlag cmd

    hasQuietFlag cmd = Shell.hasAnyFlag ["q", "quiet"] cmd
    hasOutputFlag cmd = Shell.hasAnyFlag ["o", "output-file"] cmd
    hasAppendOutputFlag cmd = Shell.hasAnyFlag ["a", "append-output"] cmd
    hasNoVerboseFlag cmd =
      Shell.hasAnyFlag ["no-verbose"] cmd
        || Shell.cmdHasArgs "wget" ["-nv"] cmd
{-# INLINEABLE rule #-}
