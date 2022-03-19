module Hadolint.Rule.DL3019 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import Language.Docker.Syntax
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Hadolint.Shell as Shell


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
    check (Run (RunArgs args flags)) = hasCacheMount flags
      || foldArguments (Shell.noCommands forgotCacheOption) args
    check _ = True
{-# INLINEABLE dl3019 #-}

hasCacheMount :: RunFlags -> Bool
hasCacheMount RunFlags { mount } =
  not (null $ Set.filter isCacheMount mount)

isCacheMount :: RunMount -> Bool
isCacheMount (CacheMount CacheOpts { cTarget = t })= isVarCacheApk t
isCacheMount _ = False

isVarCacheApk :: TargetPath -> Bool
isVarCacheApk TargetPath { unTargetPath = p }
  = Text.dropWhileEnd (=='/') p == "/var/cache/apk"

forgotCacheOption :: Shell.Command -> Bool
forgotCacheOption cmd = Shell.cmdHasArgs "apk" ["add"] cmd
  && not (Shell.hasFlag "no-cache" cmd)
