module Hadolint.Rule.DL3012 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax


rule :: Rule args
rule = customRule check (emptyState False)
  where
    code = "DL3012"
    severity = DLErrorC
    message = "Multiple `HEALTHCHECK` instructions"
    check _ st From {} = st |> replaceWith False
    check line st Healthcheck {}
        | not (state st) = st |> replaceWith True
        | otherwise = st |> addFail CheckFailure {..}
    check _ st _ = st
{-# INLINEABLE rule #-}
