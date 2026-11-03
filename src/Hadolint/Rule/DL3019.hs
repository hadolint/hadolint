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
      "Use the `--no-cache` switch to avoid the need to use `--update` and \
      \remove `/var/cache/apk/*` when done installing packages"
    check (Run (RunArgs args flags))
      | Utils.hasCacheOrTmpfsMountWith "/var/cache/apk" flags = True
      | foldArguments (Shell.noCommands forgotCacheOption) args = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3019 #-}

forgotCacheOption :: Shell.Command -> Bool
forgotCacheOption cmd = Shell.cmdHasArgs "apk" ["add"] cmd
  && not (Shell.hasFlag "no-cache" cmd)
