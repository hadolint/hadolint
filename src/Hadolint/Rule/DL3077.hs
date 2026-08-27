module Hadolint.Rule.DL3077 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3077 <> onbuild dl3077
{-# INLINEABLE rule #-}

dl3077 :: Rule Shell.ParsedShell
dl3077 = simpleRule code severity message check
  where
    code = "DL3077"
    severity = DLInfoC
    message =
      "Use BuildKit cache mount for gem/bundler (`--mount=type=cache,target=/usr/local/bundle`) \
      \to speed up Ruby gem installation"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands isGemOrBundlerCommand) args = True
      | Utils.hasCacheOrTmpfsMountWith ".gem" flags
          || Utils.hasCacheOrTmpfsMountWith "bundle" flags = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3077 #-}

isGemOrBundlerCommand :: Shell.Command -> Bool
isGemOrBundlerCommand cmd =
  Shell.cmdHasArgs "gem" ["install"] cmd
    || Shell.cmdHasArgs "bundle" ["install", "update"] cmd
