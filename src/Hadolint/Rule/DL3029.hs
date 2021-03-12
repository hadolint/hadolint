module Hadolint.Rule.DL3029 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL3029"
    severity = DLWarningC
    message = "Do not use --platform flag with FROM"

    check (From BaseImage {platform = Just p}) = p == ""
    check _ = True
{-# INLINEABLE rule #-}
