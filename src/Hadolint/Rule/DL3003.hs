module Hadolint.Rule.DL3003 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell, usingProgram)
import Language.Docker.Syntax (Instruction (..), RunArgs (..))

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3003"
    severity = DLWarningC
    message = "Use WORKDIR to switch to a directory"
    check (Run (RunArgs args _)) = foldArguments (not . usingProgram "cd") args
    check _ = True
{-# INLINEABLE rule #-}
