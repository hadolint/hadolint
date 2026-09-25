module Hadolint.Rule.DL3067 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL3067"
    severity = DLWarningC
    message = "Do not copy an entire filesystem from another stage"

    check (Copy (CopyArgs sources target) (CopyFlags _ _ _ _ (CopySource _) _)) =
      not $ any isRootSource sources && isRootTarget target
    check _ = True
{-# INLINEABLE rule #-}

isRootSource :: SourcePath -> Bool
isRootSource (SourcePath source) = dropQuotes source == "/"

isRootTarget :: TargetPath -> Bool
isRootTarget (TargetPath target) = dropQuotes target == "/"
