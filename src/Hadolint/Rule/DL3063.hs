module Hadolint.Rule.DL3063 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule Shell.ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3063"
    severity = DLWarningC
    message = "stage name should not be a reserved word"

    check (From BaseImage {alias = Just (ImageAlias als)})
      | als `elem` ["scratch", "context"] = False
      | otherwise = True
    check _ = True
{-# INLINEABLE rule #-}
