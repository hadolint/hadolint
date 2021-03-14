module Hadolint.Rule.DL4006 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule ParsedShell
rule = customRule check (emptyState False)
  where
    code = "DL4006"
    severity = DLWarningC
    message =
      "Set the SHELL option -o pipefail before RUN with a pipe in it. If you are using \
      \/bin/sh in an alpine image or if your shell is symlinked to busybox then consider \
      \explicitly setting your SHELL to /bin/ash, or disable this check"

    check _ st From {} = st |> replaceWith False -- Reset the state each time we find a new FROM
    check _ st (Shell args)
      | foldArguments isPowerShell args = st |> replaceWith True
      | otherwise = st |> replaceWith (foldArguments hasPipefailOption args)
    check line st@(State _ False) (Run (RunArgs args _))
      | foldArguments hasPipes args = st |> addFail CheckFailure {..}
      | otherwise = st
    check _ st _ = st

    isPowerShell (Shell.ParsedShell orig _ _) = "pwsh" `Text.isPrefixOf` orig
    hasPipes script = Shell.hasPipes script
    hasPipefailOption script =
      not $
        null
          [ True
            | cmd@(Shell.Command name arguments _) <- Shell.presentCommands script,
              validShell <- ["/bin/bash", "/bin/zsh", "/bin/ash", "bash", "zsh", "ash"],
              name == validShell,
              Shell.hasFlag "o" cmd,
              arg <- Shell.arg <$> arguments,
              arg == "pipefail"
          ]
{-# INLINEABLE rule #-}
