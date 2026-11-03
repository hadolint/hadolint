module Hadolint.Rule.DL3070 (rule) where

import Data.Set qualified as Set
import Data.Text qualified as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

data Acc = Empty | Acc {nodeEnvProduction :: Set.Set Text.Text}
  deriving (Show)

rule :: Rule Shell.ParsedShell
rule = DL3070 <> onbuild DL3070
{-# INLINEABLE rule #-}

-- | DL3070: Install only production dependencies for npm, yarn, and pnpm package managers
DL3070 :: Rule Shell.ParsedShell
DL3070 = customRule check (emptyState Empty)
  where
    code = "DL3070"
    severity = DLIgnoreC
    message =
      "Install only production dependencies. Use `npm ci --omit=dev`, `yarn install --production`, or `pnpm install --production`"

    check _ st (Env pairs) = st |> modify (registerEnvs pairs)
    check line st (Run (RunArgs args _))
      | foldArguments (Shell.noCommands (missingProductionFlag (state st))) args = st
      | otherwise = st |> addFail CheckFailure {..}
    check _ st _ = st
{-# INLINEABLE DL3070 #-}

missingProductionFlag :: Acc -> Shell.Command -> Bool
missingProductionFlag acc cmd =
  not (hasGlobalInstall cmd) &&
  not (hasProductionEnv acc cmd) &&
  (
    (isNpmInstallOrCi cmd && not (hasNpmProductionFlag cmd)) ||
    (isYarnInstall cmd && not (hasYarnProductionFlag cmd)) ||
    (isPnpmInstall cmd && not (hasPnpmProductionFlag cmd))
  )

registerEnvs :: Pairs -> Acc -> Acc
registerEnvs pairs (Acc envs) = Acc (Set.union envs (Set.fromList productionVars))
  where
    productionVars =
      [ Text.toUpper key
      | (key, value) <- pairs,
        Text.toUpper key == "NODE_ENV",
        Text.toLower value == "production"
      ]
registerEnvs pairs Empty = Acc (Set.fromList productionVars)
  where
    productionVars =
      [ Text.toUpper key
      | (key, value) <- pairs,
        Text.toUpper key == "NODE_ENV",
        Text.toLower value == "production"
      ]

hasProductionEnv :: Acc -> Shell.Command -> Bool
hasProductionEnv Empty _ = False
hasProductionEnv (Acc envs) cmd = Set.member "NODE_ENV" envs || any envSetArg (Shell.getArgs cmd)
  where
    envSetArg arg =
      case Text.splitOn "=" arg of
        key : valueParts ->
          let value = Text.intercalate "=" valueParts
          in Text.toUpper key == "NODE_ENV" && Text.toLower value == "production"
        _ -> False

hasGlobalInstall :: Shell.Command -> Bool
hasGlobalInstall cmd =
  Shell.hasAnyFlag ["g", "global"] cmd || "global" `elem` Shell.getArgs cmd

-- npm helpers
isNpmInstallOrCi :: Shell.Command -> Bool
isNpmInstallOrCi cmd =
  Shell.cmdHasArgs "npm" ["install", "i"] cmd || Shell.cmdHasArgs "npm" ["ci"] cmd

hasNpmProductionFlag :: Shell.Command -> Bool
hasNpmProductionFlag cmd =
  any (`elem` Shell.getArgs cmd) ["--production", "--only=production"] ||
  any (== "--omit=dev") args ||
  hasOmitDev args
  where
    args = Shell.getArgs cmd
    hasOmitDev ("--omit" : "dev" : _) = True
    hasOmitDev (_ : rest) = hasOmitDev rest
    hasOmitDev [] = False

-- yarn helpers
isYarnInstall :: Shell.Command -> Bool
isYarnInstall cmd =
  Shell.cmdHasArgs "yarn" ["install"] cmd ||
  ( Shell.name cmd == "yarn"
      && null (Shell.getArgsNoFlags cmd)
      && not (Shell.hasAnyFlag ["version", "help", "v", "h"] cmd)
  )

hasYarnProductionFlag :: Shell.Command -> Bool
hasYarnProductionFlag cmd =
  any (`elem` Shell.getArgs cmd) ["--production", "--prod"]

-- pnpm helpers
isPnpmInstall :: Shell.Command -> Bool
isPnpmInstall = Shell.cmdHasArgs "pnpm" ["install"]

hasPnpmProductionFlag :: Shell.Command -> Bool
hasPnpmProductionFlag cmd =
  any (`elem` Shell.getArgs cmd) ["--production", "--prod", "--only=production"]
