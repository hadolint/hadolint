module Hadolint.Rule.DL3071 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3071 <> onbuild dl3071
{-# INLINEABLE rule #-}

dl3071 :: Rule Shell.ParsedShell
dl3071 = simpleRule code severity message check
  where
    code = "DL3071"
    severity = DLInfoC
    message =
      "Use BuildKit cache mount for Maven (`--mount=type=cache,target=/root/.m2`) \
      \-- without it, the local Maven repository is baked into the image layer and bloats the image"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands isMavenCommand) args = True
      | Utils.hasCacheOrTmpfsMountWith ".m2" flags = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3071 #-}

isMavenCommand :: Shell.Command -> Bool
isMavenCommand cmd =
  Shell.cmdHasArgs "mvn"
    ["install", "compile", "package", "test", "verify", "deploy", "dependency:resolve"]
    cmd
