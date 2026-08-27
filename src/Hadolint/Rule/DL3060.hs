module Hadolint.Rule.DL3060 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3060 <> onbuild dl3060
{-# INLINEABLE rule #-}

dl3060 :: Rule Shell.ParsedShell
dl3060 = simpleRule code severity message check
  where
    code = "DL3060"
    severity = DLInfoC
    message =
      "Use BuildKit cache mount for yarn (`--mount=type=cache,target=/root/.cache/yarn`) \
      \or run `yarn cache clean` after `yarn install`"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands yarnInstall) args = True
      | Utils.hasCacheOrTmpfsMountWith ".cache/yarn" flags = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3060 #-}

yarnInstall :: Shell.Command -> Bool
yarnInstall = Shell.cmdHasArgs "yarn" ["install"]
