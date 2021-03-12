module Hadolint.Rule.DL3027 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3027"
    severity = DLWarningC
    message =
      "Do not use apt as it is meant to be a end-user tool, use apt-get or apt-cache instead"

    check (Run (RunArgs args _)) = foldArguments (not . Shell.usingProgram "apt") args
    check _ = True
{-# INLINEABLE rule #-}
