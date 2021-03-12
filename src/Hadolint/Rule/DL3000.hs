module Hadolint.Rule.DL3000 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax (Instruction (..))

rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL3000"
    severity = DLErrorC
    message = "Use absolute WORKDIR"
    check (Workdir loc)
      | "$" `Text.isPrefixOf` Text.dropAround dropQuotes loc = True
      | "/" `Text.isPrefixOf` Text.dropAround dropQuotes loc = True
      | otherwise = False
    check _ = True
    dropQuotes chr
      | chr == '\"' = True
      | chr == '\'' = True
      | otherwise = False
{-# INLINEABLE rule #-}
