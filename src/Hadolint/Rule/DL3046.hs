module Hadolint.Rule.DL3046 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax (Instruction (..), RunArgs (..))

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3046"
    severity = DLWarningC
    message = "`useradd` without flag `-l` and high UID will result in excessively large Image."

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotFlagL) args
    check _ = True

    forgotFlagL cmd = isUseradd cmd && (not (hasLFlag cmd) && hasUFlag cmd && hasLongUID cmd)
    isUseradd (Shell.Command name _ _) = name == "useradd"
    hasLFlag = Shell.hasAnyFlag ["l", "no-log-init"]
    hasUFlag = Shell.hasAnyFlag ["u", "uid"]
    hasLongUID cmd = any ((> 5) . Text.length) (Shell.getFlagArg "u" cmd)
{-# INLINEABLE rule #-}
