module Hadolint.Rule.DL3047 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax


rule :: Rule ParsedShell
rule = dl3047 <> onbuild dl3047
{-# INLINEABLE rule #-}

dl3047 :: Rule ParsedShell
dl3047 = simpleRule code severity message check
  where
    code = "DL3047"
    severity = DLInfoC
    message =
      "Avoid use of wget without progress bar. Use `wget --progress=dot:giga <url>`. \
      \Or consider using `-q` or `-nv` (shorthands for `--quiet` or `--no-verbose`)."

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotProgress) args
    check _ = True

    forgotProgress cmd = isWget cmd && (not (hasProgressOption cmd) && not (hasSpecialFlags cmd))
    isWget (Shell.Command name _ _) = name == "wget"
    hasProgressOption = Shell.hasFlag "progress"

    hasSpecialFlags cmd =
      hasQuietFlag cmd
        || hasOutputFlag cmd
        || hasAppendOutputFlag cmd
        || hasNoVerboseFlag cmd

    hasQuietFlag = Shell.hasAnyFlag ["q", "quiet"]
    hasOutputFlag = Shell.hasAnyFlag ["o", "output-file"]
    hasAppendOutputFlag = Shell.hasAnyFlag ["a", "append-output"]
    hasNoVerboseFlag cmd =
      Shell.hasAnyFlag ["no-verbose"] cmd
        || Shell.cmdHasArgs "wget" ["-nv"] cmd
{-# INLINEABLE dl3047 #-}
