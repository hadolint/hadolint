module Hadolint.Rule.DL3035 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule Shell.ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3035"
    severity = DLWarningC
    message = "Do not use `zypper update`."

    check (Run (RunArgs args _)) =
      foldArguments
        ( Shell.noCommands
            ( Shell.cmdHasArgs
                "zypper"
                [ "update",
                  "up",
                  "dist-upgrade",
                  "dup"
                ]
            )
        )
        args
    check _ = True
{-# INLINEABLE rule #-}
