module Hadolint.Rule.DL3042 (rule) where

import Data.List (isInfixOf)
import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule Shell.ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3042"
    severity = DLWarningC
    message =
      "Avoid use of cache directory with pip. Use `pip install --no-cache-dir <package>`"
    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotNoCacheDir) args
    check _ = True
    forgotNoCacheDir cmd =
      Shell.isPipInstall cmd && not (usesNoCacheDir cmd) && not (isPipWrapper cmd)
    usesNoCacheDir cmd = "--no-cache-dir" `elem` Shell.getArgs cmd
{-# INLINEABLE rule #-}

isPipWrapper :: Shell.Command -> Bool
isPipWrapper cmd@(Shell.Command name _ _) = isWrapper "pipx" || isWrapper "pipenv"
  where
    isWrapper :: Text.Text -> Bool
    isWrapper w =
      w `Text.isInfixOf` name
        || ("python" `Text.isPrefixOf` name && ["-m", w] `isInfixOf` Shell.getArgs cmd)
