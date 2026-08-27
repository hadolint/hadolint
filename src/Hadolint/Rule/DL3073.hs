module Hadolint.Rule.DL3073 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3073 <> onbuild dl3073
{-# INLINEABLE rule #-}

dl3073 :: Rule Shell.ParsedShell
dl3073 = simpleRule code severity message check
  where
    code = "DL3073"
    severity = DLInfoC
    message =
      "Use BuildKit cache mount for Composer (`--mount=type=cache,target=/root/.composer/cache`) \
      \to speed up PHP dependency installation"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands isComposerCommand) args = True
      | Utils.hasCacheOrTmpfsMountWith ".composer/cache" flags = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3073 #-}

isComposerCommand :: Shell.Command -> Bool
isComposerCommand cmd =
  Shell.cmdHasArgs "composer" ["install", "update", "require"] cmd
