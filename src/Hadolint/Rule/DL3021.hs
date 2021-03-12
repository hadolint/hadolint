module Hadolint.Rule.DL3021 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL3021"
    severity = DLErrorC
    message = "COPY with more than 2 arguments requires the last argument to end with /"

    check (Copy (CopyArgs sources t _ _))
      | length sources > 1 = endsWithSlash t
      | otherwise = True
    check _ = True

    endsWithSlash (TargetPath t) = not (Text.null t) && Text.last t == '/'
{-# INLINEABLE rule #-}
