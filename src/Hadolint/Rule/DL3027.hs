module Hadolint.Rule.DL3027 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell, usingProgram)
import Language.Docker.Syntax


rule :: Rule ParsedShell
rule = dl3027 <> onbuild dl3027
{-# INLINEABLE rule #-}

dl3027 :: Rule ParsedShell
dl3027 = simpleRule code severity message check
  where
    code = "DL3027"
    severity = DLWarningC
    message = "Do not use apt as it is meant to be an end-user tool, use apt-get\
              \ or apt-cache instead"

    check (Run (RunArgs args _)) = foldArguments (not . usingProgram "apt") args
    check _ = True
{-# INLINEABLE dl3027 #-}
