module Hadolint.Rule.DL3062 (dl3062) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

-- | DL3062: Install only essential dependencies. Instead of `npm ci/install` use `npm --production ci/install`
dl3062 :: Rule ParsedShell
dl3062 = simpleRule code severity message check
  where
    code = "DL3062"
    severity = DLWarningC
    message =
      "Install only essential dependencies. Instead of `npm ci/install` use `npm --production ci/install`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands missingProductionFlag) args
    check _ = True

missingProductionFlag :: Shell.Command -> Bool
missingProductionFlag cmd =
  (isNpmInstall cmd || isNpmCi cmd) && not (hasProductionFlag cmd)

isNpmInstall :: Shell.Command -> Bool
isNpmInstall cmd = Shell.cmdHasArgs cmd "npm" ["install"]

isNpmCi :: Shell.Command -> Bool
isNpmCi cmd = Shell.cmdHasArgs cmd "npm" ["ci"]

hasProductionFlag :: Shell.Command -> Bool
hasProductionFlag cmd =
  Shell.cmdHasArgs cmd "npm" ["--production"] ||
  Shell.cmdHasArgs cmd "npm" ["--omit=dev"]
