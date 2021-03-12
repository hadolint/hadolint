module Hadolint.Rule.DL3023 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = customRule check (emptyState Nothing)
  where
    code = "DL3023"
    severity = DLErrorC
    message = "COPY --from should reference a previously defined FROM alias"

    check _ st f@(From _) = st |> replaceWith (Just f) -- Remember the last FROM instruction found
    check line st@(State _ (Just fromInstr)) (Copy (CopyArgs _ _ _ (CopySource stageName)))
      | aliasMustBe (/= stageName) fromInstr = st
      | otherwise = st |> addFail CheckFailure {..}
    -- cannot copy from the same stage!
    check _ st _ = st
{-# INLINEABLE rule #-}
