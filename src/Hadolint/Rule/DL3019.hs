module Hadolint.Rule.DL3019 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import Language.Docker.Syntax
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils


rule :: Rule ParsedShell
rule = dl3019 <> onbuild dl3019
{-# INLINEABLE rule #-}

dl3019 :: Rule ParsedShell
dl3019 = simpleRule code severity message check
  where
    code = "DL3019"
    severity = DLInfoC
    message =
      "Use BuildKit cache mount for apk (`--mount=type=cache,target=/var/cache/apk`) \
      \-- without it, cached package files are baked into the image layer and bloat the image"
    check (Run (RunArgs args flags))
      | Utils.hasCacheOrTmpfsMountWith "/var/cache/apk" flags = True
      | foldArguments (Shell.noCommands hasApkAdd) args = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3019 #-}

hasApkAdd :: Shell.Command -> Bool
hasApkAdd cmd = Shell.cmdHasArgs "apk" ["add"] cmd
