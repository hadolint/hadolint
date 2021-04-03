module Hadolint.Rule.DL3060 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3060"
    severity = DLInfoC
    message = "`yarn cache clean` missing after `yarn install` was run."

    check (Run (RunArgs args _)) =
      foldArguments (Shell.noCommands yarnInstall) args
        || ( foldArguments (Shell.anyCommands yarnInstall) args
              && foldArguments (Shell.anyCommands yarnCacheClean) args
           )
    check _ = True
{-# INLINEABLE rule #-}

yarnInstall :: Shell.Command -> Bool
yarnInstall = Shell.cmdHasArgs "yarn" ["install"]

yarnCacheClean :: Shell.Command -> Bool
yarnCacheClean = Shell.cmdHasArgs "yarn" ["cache", "clean"]
