module Hadolint.Rule.DL3005 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax (Instruction (..), RunArgs (..))

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3005"
    severity = DLErrorC
    message = "Do not use apt-get dist-upgrade"
    check (Run (RunArgs args _)) =
      foldArguments (Shell.noCommands (Shell.cmdHasArgs "apt-get" ["dist-upgrade"])) args
    check _ = True
{-# INLINEABLE rule #-}
