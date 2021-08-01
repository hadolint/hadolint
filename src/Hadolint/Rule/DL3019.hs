module Hadolint.Rule.DL3019 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3019"
    severity = DLInfoC
    message =
      "Use the `--no-cache` switch to avoid the need to use `--update` and \
      \remove `/var/cache/apk/*` when done installing packages"
    check (Run (RunArgs args flags)) = hasCacheMount flags
      || foldArguments (Shell.noCommands forgotCacheOption) args
    check _ = True
{-# INLINEABLE rule #-}

hasCacheMount :: RunFlags -> Bool
hasCacheMount RunFlags
  { mount =
      Just (CacheMount CacheOpts {cTarget = TargetPath {unTargetPath = p}})
  } = Text.dropWhileEnd (=='/') p == "/var/cache/apk"
hasCacheMount RunFlags {} = False

forgotCacheOption :: Shell.Command -> Bool
forgotCacheOption cmd = Shell.cmdHasArgs "apk" ["add"] cmd
  && not (Shell.hasFlag "no-cache" cmd)
