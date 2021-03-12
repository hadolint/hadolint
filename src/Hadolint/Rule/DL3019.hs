module Hadolint.Rule.DL3019 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3019"
    severity = DLInfoC
    message =
      "Use the `--no-cache` switch to avoid the need to use `--update` and remove \
      \`/var/cache/apk/*` when done installing packages"
    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotCacheOption) args
    check _ = True
    forgotCacheOption cmd = Shell.cmdHasArgs "apk" ["add"] cmd && not (Shell.hasFlag "no-cache" cmd)
{-# INLINEABLE rule #-}
