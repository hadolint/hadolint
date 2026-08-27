module Hadolint.Rule.DL3075 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3075 <> onbuild dl3075
{-# INLINEABLE rule #-}

dl3075 :: Rule Shell.ParsedShell
dl3075 = simpleRule code severity message check
  where
    code = "DL3075"
    severity = DLInfoC
    message =
      "Use BuildKit cache mount for pnpm (`--mount=type=cache,target=/root/.local/share/pnpm/store`) \
      \-- without it, the pnpm store is baked into the image layer and bloats the image"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands isPnpmCommand) args = True
      | Utils.hasCacheOrTmpfsMountWith "pnpm" flags = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3075 #-}

isPnpmCommand :: Shell.Command -> Bool
isPnpmCommand cmd =
  Shell.cmdHasArgs "pnpm" ["install", "add", "fetch", "i"] cmd
