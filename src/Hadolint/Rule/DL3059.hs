module Hadolint.Rule.DL3059 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax


type Acc = Int

rule :: Rule args
rule = customRule check (emptyState 0)
  where
    code = "DL3059"
    severity = DLInfoC
    message = "Multiple consecutive `RUN` instructions. Consider consolidation."

    check line st (Run _)
      | state st < 1 = st |> modify remember
      | otherwise = st |> addFail CheckFailure {..}
    check _ st _ = st |> modify reset
{-# INLINEABLE rule #-}

remember :: Acc -> Acc
remember a = a + 1

reset :: Acc -> Acc
reset _ = 0
