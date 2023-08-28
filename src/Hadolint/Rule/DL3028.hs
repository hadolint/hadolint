module Hadolint.Rule.DL3028 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax (Instruction (..), RunArgs (..))


rule :: Rule ParsedShell
rule = dl3028 <> onbuild dl3028
{-# INLINEABLE rule #-}

dl3028 :: Rule ParsedShell
dl3028 = simpleRule code severity message check
  where
    code = "DL3028"
    severity = DLWarningC
    message =
      "Pin versions in gem install. Instead of `gem install <gem>` use `gem \
      \install <gem>:<version>`"

    check (Run (RunArgs args _)) = foldArguments (all versionFixed . gems) args
    check _ = True

    versionFixed package = ":" `Text.isInfixOf` package
{-# INLINEABLE dl3028 #-}

gems :: Shell.ParsedShell -> [Text.Text]
gems shell =
  [ arg
    | cmd <- Shell.presentCommands shell,
      Shell.cmdHasArgs "gem" ["install", "i"] cmd,
      not (Shell.cmdHasArgs "gem" ["-v"] cmd),
      not (Shell.cmdHasArgs "gem" ["--version"] cmd),
      not (Shell.cmdHasPrefixArg "gem" "--version=" cmd),
      let args = Shell.getArgs cmd,
      let argsUntilDoubleDash = takeWhile (/= "--") args,
      arg <- removeOptions argsUntilDoubleDash,
      arg /= "install",
      arg /= "i"
  ]
  where
    removeOptions [] = []
    removeOptions (x : xs)
      | x == "--" = removeOptions xs
      | "-" `Text.isPrefixOf` x = removeOptions (drop 1 xs)
      | otherwise = x : removeOptions xs
