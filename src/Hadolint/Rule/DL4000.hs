module Hadolint.Rule.DL4000 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax (Instruction (..))

rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL4000"
    severity = DLErrorC
    message = "MAINTAINER is deprecated"
    check (Maintainer _) = False
    check _ = True
{-# INLINEABLE rule #-}
