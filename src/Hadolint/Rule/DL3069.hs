module Hadolint.Rule.DL3069 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3069 <> onbuild dl3069
{-# INLINEABLE rule #-}

dl3069 :: Rule Shell.ParsedShell
dl3069 = simpleRule code severity message check
  where
    code = "DL3069"
    severity = DLInfoC
    message =
      "Use BuildKit cache mounts for cargo (`--mount=type=cache,target=/root/.cargo/registry \
      \--mount=type=cache,target=/root/.cargo/git`) to speed up Rust builds"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands isCargoCommand) args = True
      | Utils.hasCacheOrTmpfsMountWith ".cargo" flags
          || ( Utils.hasCacheOrTmpfsMountWith ".cargo/registry" flags
                && Utils.hasCacheOrTmpfsMountWith ".cargo/git" flags ) = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3069 #-}

isCargoCommand :: Shell.Command -> Bool
isCargoCommand cmd =
  Shell.cmdHasArgs "cargo" ["build", "install", "fetch", "test", "check", "clippy"] cmd
