module Hadolint.Rule.DL3032 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3032 <> onbuild dl3032
{-# INLINEABLE rule #-}

dl3032 :: Rule Shell.ParsedShell
dl3032 = simpleRule code severity message check
  where
    code = "DL3032"
    severity = DLWarningC
    message = "`yum clean all` missing after yum command."

    check (Run (RunArgs args _)) =
      foldArguments (Shell.noCommands yumInstall) args
        || ( foldArguments (Shell.anyCommands yumInstall) args
               && foldArguments (Shell.anyCommands yumClean) args
           )
    check _ = True

    yumInstall = Shell.cmdHasArgs "yum" ["install"]
    yumClean args = Shell.cmdHasArgs "yum" ["clean", "all"] args
      || Shell.cmdHasArgs "rm" ["-rf", "/var/cache/yum/*"] args
{-# INLINEABLE dl3032 #-}
