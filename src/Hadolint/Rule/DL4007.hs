module Hadolint.Rule.DL4007 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL4007"
    severity = DLWarningC
    message =
      "COPY with '.' as source can leak sensitive files like .git, .env, or \
      \local configs into your image. Prefer copying only the files you actually need."

    check (Copy (CopyArgs sources _) _)
      | any isDotSource sources = False
      | otherwise = True
    check _ = True

    isDotSource src =
      let raw = unSourcePath src
          unquoted = Text.dropAround (== '"') . Text.dropAround (== '\'') $ raw
      in unquoted == "." || unquoted == "./"
{-# INLINEABLE rule #-}
