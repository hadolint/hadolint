module Hadolint.Rule.DL3070 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3070 <> onbuild dl3070
{-# INLINEABLE rule #-}

dl3070 :: Rule Shell.ParsedShell
dl3070 = simpleRule code severity message check
  where
    code = "DL3070"
    severity = DLInfoC
    message =
      "Use BuildKit cache mounts for Go (`--mount=type=cache,target=/root/.cache/go-build \
      \--mount=type=cache,target=/go/pkg/mod`) -- without them, the Go build and module \
      \caches are baked into the image layer and bloat the image"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands isGoCommand) args = True
      | Utils.hasCacheOrTmpfsMountWith ".cache/go-build" flags
          || Utils.hasCacheOrTmpfsMountWith "go/pkg/mod" flags = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3070 #-}

isGoCommand :: Shell.Command -> Bool
isGoCommand cmd =
  Shell.cmdHasArgs "go" ["build", "get", "install", "test", "mod"] cmd
