module Hadolint.Rule.DL3010 (rule) where

import Data.Foldable (toList)
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL3010"
    severity = DLInfoC
    message = "Use ADD for extracting archives into an image"
    check (Copy (CopyArgs srcs _ _ _ NoSource)) =
      and
        [ not (format `Text.isSuffixOf` src)
          | SourcePath src <- toList srcs,
            format <- archiveFileFormatExtensions
        ]
    check _ = True
{-# INLINEABLE rule #-}
