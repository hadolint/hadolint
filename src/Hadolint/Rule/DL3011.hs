module Hadolint.Rule.DL3011 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL3011"
    severity = DLErrorC
    message = "Valid UNIX ports range from 0 to 65535"
    check (Expose (Ports ports)) =
      and [p <= 65535 | Port p _ <- ports]
        && and [l <= 65535 && m <= 65535 | PortRange l m _ <- ports]
    check _ = True
{-# INLINEABLE rule #-}
