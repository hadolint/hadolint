module Hadolint.Rule.DL3065 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL3065"
    severity = DLWarningC
    message =
      "Setting `FROM --platform` to predefined `$TARGETPLATFORM` in is\
      \ redundant as this is the default behavior"

    check (From (BaseImage _ _ _ _ (Just platform))) =
      platform /= "$TARGETPLATFORM" && platform /= "${TARGETPLATFORM}"
    check _ = True
{-# INLINEABLE rule #-}
