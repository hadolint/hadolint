module Hadolint.Rule.DL3029 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL3029"
    severity = DLWarningC
    message = "Do not use --platform flag with FROM"

    check (From BaseImage {platform = Just p}) = "BUILDPLATFORM" `Text.isInfixOf` p || "TARGETPLATFORM" `Text.isInfixOf` p
    check _ = True
{-# INLINEABLE rule #-}
