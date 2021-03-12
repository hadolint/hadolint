module Hadolint.Rule.DL3025 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL3025"
    severity = DLWarningC
    message = "Use arguments JSON notation for CMD and ENTRYPOINT arguments"

    check (Cmd (ArgumentsText _)) = False
    check (Entrypoint (ArgumentsText _)) = False
    check _ = True
{-# INLINEABLE rule #-}
