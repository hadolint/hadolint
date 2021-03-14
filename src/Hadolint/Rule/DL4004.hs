module Hadolint.Rule.DL4004 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax (Instruction (..))

data HasEntry = HasEntry | NoEntry

rule :: Rule args
rule = customRule check (emptyState NoEntry)
  where
    code = "DL4004"
    severity = DLErrorC
    message =
      "Multiple `ENTRYPOINT` instructions found. If you list more than one `ENTRYPOINT` then \
      \only the last `ENTRYPOINT` will take effect"

    -- Reset the state each time we find a FROM
    check _ st From {} = st |> replaceWith NoEntry
    -- Remember we found an ENTRYPOINT
    check _ st@(State _ NoEntry) Entrypoint {} = st |> replaceWith HasEntry
    -- Add a failure if we found another entrypoint in the same stage
    check line st@(State _ HasEntry) Entrypoint {} = st |> addFail (CheckFailure {..})
    check _ st _ = st
{-# INLINEABLE rule #-}
