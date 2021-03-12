module Hadolint.Rule.DL3039 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule Shell.ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3039"
    severity = DLErrorC
    message = "Do not use dnf update."

    check (Run (RunArgs args _)) =
      foldArguments
        ( Shell.noCommands
            ( Shell.cmdHasArgs
                "dnf"
                [ "upgrade",
                  "upgrade-minimal"
                ]
            )
        )
        args
    check _ = True
{-# INLINEABLE rule #-}
