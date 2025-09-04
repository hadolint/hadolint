module Hadolint.Rule.DL3062 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax
import qualified Data.Text as Text

rule :: Rule Shell.ParsedShell
rule = dl3062 <> onbuild dl3062
{-# INLINEABLE rule #-}

-- | DL3062: Install only production dependencies for npm, yarn, and pnpm package managers
dl3062 :: Rule Shell.ParsedShell
dl3062 = simpleRule code severity message check
  where
    code = "DL3062"
    severity = DLWarningC
    message =
      "Install only production dependencies. Use `npm ci --production`, `yarn install --production`, or `pnpm install --production`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands missingProductionFlag) args
    check _ = True
{-# INLINEABLE dl3062 #-}

missingProductionFlag :: Shell.Command -> Bool
missingProductionFlag cmd =
  (isNpmInstallOrCi cmd && not (hasNpmProductionFlag cmd)) ||
  (isYarnInstall cmd && not (hasYarnProductionFlag cmd)) ||
  (isPnpmInstallOrCi cmd && not (hasPnpmProductionFlag cmd))

-- npm helpers
isNpmInstallOrCi :: Shell.Command -> Bool
isNpmInstallOrCi cmd = Shell.cmdHasArgs "npm" ["install"] cmd || Shell.cmdHasArgs "npm" ["ci"] cmd

hasNpmProductionFlag :: Shell.Command -> Bool
hasNpmProductionFlag cmd =
  any (`elem` Shell.getArgs cmd) ["--production", "--only=production"] ||
  any (\arg -> "--omit=dev" `Text.isPrefixOf` arg) (Shell.getArgs cmd)

-- yarn helpers
isYarnInstall :: Shell.Command -> Bool
isYarnInstall = Shell.cmdHasArgs "yarn" ["install"]

hasYarnProductionFlag :: Shell.Command -> Bool
hasYarnProductionFlag cmd =
  any (`elem` Shell.getArgs cmd) ["--production", "--prod"]

-- pnpm helpers
isPnpmInstallOrCi :: Shell.Command -> Bool
isPnpmInstallOrCi cmd = Shell.cmdHasArgs "pnpm" ["install"] cmd || Shell.cmdHasArgs "pnpm" ["ci"] cmd

hasPnpmProductionFlag :: Shell.Command -> Bool
hasPnpmProductionFlag cmd =
  any (`elem` Shell.getArgs cmd) ["--production", "--prod", "--only=production"]
