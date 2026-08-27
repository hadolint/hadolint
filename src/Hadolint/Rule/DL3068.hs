module Hadolint.Rule.DL3068 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3068 <> onbuild dl3068
{-# INLINEABLE rule #-}

dl3068 :: Rule Shell.ParsedShell
dl3068 = simpleRule code severity message check
  where
    code = "DL3068"
    severity = DLInfoC
    message =
      "Use BuildKit cache mount for npm (`--mount=type=cache,target=/root/.npm`) \
      \-- without it, the npm cache is baked into the image layer and bloats the image"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands isNpmCommand) args = True
      | Utils.hasCacheOrTmpfsMountWith ".npm" flags = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3068 #-}

isNpmCommand :: Shell.Command -> Bool
isNpmCommand cmd =
  Shell.cmdHasArgs "npm" ["install", "ci", "install-ci-test", "it"] cmd
