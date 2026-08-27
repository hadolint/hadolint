module Hadolint.Rule.DL3042 (rule) where

import Data.List (isInfixOf)
import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3042 <> onbuild dl3042
{-# INLINEABLE rule #-}

dl3042 :: Rule Shell.ParsedShell
dl3042 = simpleRule code severity message check
  where
    code = "DL3042"
    severity = DLWarningC
    message =
      "Use BuildKit cache mount for pip (`--mount=type=cache,target=/root/.cache/pip`) \
      \or use `pip install --no-cache-dir <package>` to avoid cache bloat"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands isPipInstallCommand) args = True
      | Utils.hasCacheOrTmpfsMountWith ".cache/pip" flags = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3042 #-}

isPipInstallCommand :: Shell.Command -> Bool
isPipInstallCommand cmd = Shell.isPipInstall cmd && not (isPipWrapper cmd)

isPipWrapper :: Shell.Command -> Bool
isPipWrapper cmd@(Shell.Command name _ _) = isWrapper "pipx" || isWrapper "pipenv"
  where
    isWrapper :: Text.Text -> Bool
    isWrapper w =
      w `Text.isInfixOf` name
        || ("python" `Text.isPrefixOf` name && ["-m", w] `isInfixOf` Shell.getArgs cmd)
