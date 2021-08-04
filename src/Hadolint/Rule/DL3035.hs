module Hadolint.Rule.DL3035 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule Shell.ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3035"
    severity = DLWarningC
    message = "Do not use `zypper dist-upgrade`."

    check (Run (RunArgs args _)) =
      foldArguments
        ( Shell.noCommands
            ( Shell.cmdHasArgs
                "zypper"
                [ "dist-upgrade",
                  "dup"
                ]
            )
        )
        args
    check _ = True
{-# INLINEABLE rule #-}
