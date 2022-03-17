module Hadolint.Rule.DL3061 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax


rule :: Rule args
rule = customRule check (emptyState False)
  where
    code = "DL3061"
    severity = DLErrorC
    message = "Invalid instruction order. Dockerfile must begin with `FROM`,\
              \ `ARG` or comment."

    check _ st (From _) = st |> replaceWith True
    check _ st (Comment _) = st
    check _ st (Pragma _) = st
    check _ st (Arg _ _) = st
    check line st _
      | state st = st
      | otherwise = st |> addFail CheckFailure {..}
{-# INLINEABLE rule #-}
