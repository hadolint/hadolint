module Hadolint.Rule.DL3059 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax


data Acc
  = Acc RunFlags
  | Empty
  deriving (Eq, Show)

rule :: Rule args
rule = customRule check (emptyState Empty)
  where
    code = "DL3059"
    severity = DLInfoC
    message = "Multiple consecutive `RUN` instructions. Consider consolidation."

    check line st (Run (RunArgs _ flags))
      | state st == Acc flags = st |> addFail CheckFailure {..}
      | otherwise = st |> modify (remember flags)
    check _ st (Comment _) = st
    check _ st _ = st |> modify reset
{-# INLINEABLE rule #-}

remember :: RunFlags -> Acc -> Acc
remember flags _ = Acc flags

reset :: Acc -> Acc
reset _ = Empty
