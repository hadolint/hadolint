module Hadolint.Rule.DL3062 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule Shell.ParsedShell
rule = dl3062 <> onbuild dl3062
{-# INLINEABLE rule #-}

-- | DL3062: Install only essential dependencies. Instead of `npm ci/install` use `npm --production ci/install`
dl3062 :: Rule Shell.ParsedShell
dl3062 = simpleRule code severity message check
  where
    code = "DL3062"
    severity = DLWarningC
    message =
      "Install only essential dependencies. Instead of `npm ci/install` use `npm --production ci/install`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands missingProductionFlag) args
    check _ = True
{-# INLINEABLE dl3062 #-}

missingProductionFlag :: Shell.Command -> Bool
missingProductionFlag cmd =
  (isNpmInstall cmd || isNpmCi cmd) && not (hasProductionFlag cmd)

isNpmInstall :: Shell.Command -> Bool
isNpmInstall = Shell.cmdHasArgs "npm" ["install"]

isNpmCi :: Shell.Command -> Bool
isNpmCi = Shell.cmdHasArgs "npm" ["ci"]

hasProductionFlag :: Shell.Command -> Bool
hasProductionFlag cmd =
  Shell.hasFlag "--production" cmd || Shell.hasFlag "--omit=dev" cmd
