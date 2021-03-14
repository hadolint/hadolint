module Hadolint.Rule.DL3004 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell, usingProgram)
import Language.Docker.Syntax (Instruction (..), RunArgs (..))

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3004"
    severity = DLErrorC
    message =
      "Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce \
      \root"
    check (Run (RunArgs args _)) = foldArguments (not . usingProgram "sudo") args
    check _ = True
{-# INLINEABLE rule #-}
