module Hadolint.Rule.DL3031 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule Shell.ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3031"
    severity = DLErrorC
    message = "Do not use yum update."
    check (Run (RunArgs args _)) =
      foldArguments
        ( Shell.noCommands
            ( Shell.cmdHasArgs
                "yum"
                [ "update",
                  "update-to",
                  "upgrade",
                  "upgrade-to"
                ]
            )
        )
        args
    check _ = True
{-# INLINEABLE rule #-}
