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

    check line st (From BaseImage {alias = Just (ImageAlias als)}) =
      let newState = st |> modify (Set.insert als)
       in if Set.member als (state st)
            then newState |> addFail CheckFailure {..}
            else newState
    check _ st _ = st
{-# INLINEABLE rule #-}
