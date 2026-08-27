module Hadolint.Rule.DL3076 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3076 <> onbuild dl3076
{-# INLINEABLE rule #-}

dl3076 :: Rule Shell.ParsedShell
dl3076 = simpleRule code severity message check
  where
    code = "DL3076"
    severity = DLInfoC
    message =
      "Use BuildKit cache mounts for Mix (`--mount=type=cache,target=/root/.hex \
      \--mount=type=cache,target=/root/.mix`) to speed up Elixir dependency fetching"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands isMixCommand) args = True
      | Utils.hasCacheOrTmpfsMountWith ".hex" flags
          || Utils.hasCacheOrTmpfsMountWith ".mix" flags = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3076 #-}

isMixCommand :: Shell.Command -> Bool
isMixCommand cmd =
  Shell.cmdHasArgs "mix" ["deps.get", "deps.compile", "compile"] cmd
