module Hadolint.Rule.DL4006 (rule) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

-- | Whether pipes are safe in the current stage, and the same flag for every
-- named stage so that stages built on top of them inherit their `SHELL`.
data Acc = Acc
  { current :: Maybe Text,
    pipefail :: Bool,
    stages :: Map Text Bool
  }

rule :: Rule ParsedShell
rule = customRule check (emptyState (Acc Nothing False Map.empty))
  where
    code = "DL4006"
    severity = DLWarningC
    message =
      "Set the SHELL option -o pipefail before RUN with a pipe in it. If you are using \
      \/bin/sh in an alpine image or if your shell is symlinked to busybox then consider \
      \explicitly setting your SHELL to /bin/ash, or disable this check"

    check _ st (From BaseImage {image, alias}) = st |> modify (enterStage image alias)
    check _ st (Shell args) =
      st |> modify (setPipefail (foldArguments isNonPosixShell args || foldArguments hasPipefailOption args))
    check line st@(State _ Acc {pipefail = False}) (Run (RunArgs args _))
      | foldArguments Shell.hasPipes args = st |> addFail CheckFailure {..}
      | otherwise = st
    check _ st _ = st

    isNonPosixShell (Shell.ParsedShell orig _ _) =
      any (`Text.isPrefixOf` orig) Shell.nonPosixShells
    hasPipefailOption script =
      not $
        null
          [ True
            | cmd@(Shell.Command name arguments _) <- Shell.presentCommands script,
              Shell.shellBasename name `elem` ["bash", "zsh", "ash"],
              Shell.hasFlag "o" cmd,
              arg <- Shell.arg <$> arguments,
              arg == "pipefail"
          ]
{-# INLINEABLE rule #-}

enterStage :: Image -> Maybe ImageAlias -> Acc -> Acc
enterStage Image {registryName, imageName} als Acc {stages} =
  setPipefail inherited Acc {current = unImageAlias <$> als, pipefail = False, stages}
  where
    inherited = case registryName of
      Nothing -> Map.findWithDefault False imageName stages
      Just _ -> False

setPipefail :: Bool -> Acc -> Acc
setPipefail value acc@Acc {current} =
  acc
    { pipefail = value,
      stages = maybe (stages acc) (\name -> Map.insert name value (stages acc)) current
    }
