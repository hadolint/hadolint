module Hadolint.Rule.DL3017 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3017"
    severity = DLErrorC
    message = "Do not use apk upgrade"
    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands (Shell.cmdHasArgs "apk" ["upgrade"])) args
    check _ = True
{-# INLINEABLE rule #-}
