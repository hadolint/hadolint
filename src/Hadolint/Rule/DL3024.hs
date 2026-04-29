module Hadolint.Rule.DL3024 (rule) where

import qualified Data.Set as Set
import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = customRule check (emptyState Set.empty)
  where
    code = "DL3024"
    severity = DLErrorC
    message = "FROM aliases (stage names) must be unique"

    check line st (From BaseImage {alias = Just (ImageAlias als)})
      | Set.member als (state st) = st |> addFail CheckFailure {..}
      | otherwise = st |> modify (Set.insert als)
    check _ st _ = st
{-# INLINEABLE rule #-}
