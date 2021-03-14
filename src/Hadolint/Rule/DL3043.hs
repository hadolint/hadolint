module Hadolint.Rule.DL3043 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL3043"
    severity = DLErrorC
    message = "`ONBUILD`, `FROM` or `MAINTAINER` triggered from within `ONBUILD` instruction."

    check (OnBuild (OnBuild _)) = False
    check (OnBuild (From _)) = False
    check (OnBuild (Maintainer _)) = False
    check _ = True
{-# INLINEABLE rule #-}
