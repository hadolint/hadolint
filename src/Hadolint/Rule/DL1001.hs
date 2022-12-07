module Hadolint.Rule.DL1001 (rule) where

import Hadolint.Pragma
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import Language.Docker.Syntax


rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL1001"
    severity = DLIgnoreC
    message = "Please refrain from using inline ignore pragmas \
              \ `# hadolint ignore=DLxxxx`."
    check (Comment com) =
      case parseIgnorePragma com of
        Just _ -> False
        _ -> True
    check _ = True
{-# INLINEABLE rule #-}
