module Hadolint.Rule.DL4003 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax (Instruction (..))

data HasCmd = HasCmd | NoCmd

rule :: Rule args
rule = customRule check (emptyState NoCmd)
  where
    code = "DL4003"
    severity = DLWarningC
    message =
      "Multiple `CMD` instructions found. If you list more than one `CMD` then only the last \
      \`CMD` will take effect"

    -- Reset the state each time we find a FROM
    check _ st From {} = st |> replaceWith NoCmd
    -- Remember we found a CMD, fail if we found a CMD before
    check _ st@(State _ NoCmd) Cmd {} = st |> replaceWith HasCmd
    -- If we already saw a CMD, add an error for the line
    check line st@(State _ HasCmd) Cmd {} = st |> addFail (CheckFailure {..})
    -- otherwise return the accumulator
    check _ st _ = st
{-# INLINEABLE rule #-}
