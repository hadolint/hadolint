module Hadolint.Rule.DL3022 (rule) where

import qualified Data.Set as Set
import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = customRule check (emptyState Set.empty)
  where
    code = "DL3022"
    severity = DLWarningC
    message = "COPY --from should reference a previously defined FROM alias"

    check _ st (From BaseImage {alias = Just (ImageAlias als)}) = st |> modify (Set.insert als)
    check line st (Copy (CopyArgs _ _ _ (CopySource s)))
      | Set.member s (state st) = st
      | otherwise = st |> addFail CheckFailure {..}
    check _ st _ = st
{-# INLINEABLE rule #-}
