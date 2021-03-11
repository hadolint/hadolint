module Hadolint.Rule.DL3009 (rule) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

data Acc
  = Acc
      { lastFrom :: BaseImage,
        stages :: Map.Map Text.Text Linenumber,
        forgets :: Map.Map Linenumber BaseImage
      }
  | Empty

rule :: Rule Shell.ParsedShell
rule = veryCustomRule check (emptyState Empty) markFailures
  where
    code = "DL3009"
    severity = DLInfoC
    message = "Delete the apt-get lists after installing something"

    check line st (From from) = st |> modify (rememberStage line from)
    check line st (Run (RunArgs args _))
      | foldArguments forgotToCleanup args = st |> modify (rememberLine line)
      | otherwise = st
    check _ st _ = st

    -- Convert the final state into failures
    markFailures (State fails Empty) = fails
    markFailures (State _ Acc {..}) =
      forgets
        |> Map.foldMapWithKey mapFail
      where
        mapFail line from
          | from == lastFrom = mempty
          | BaseImage {alias = Just (ImageAlias als)} <- from, -- Check if this stage is used later
            Just _ <- Map.lookup als stages =
            -- If the same alias is used in another stage, fail
            Seq.singleton CheckFailure {..}
          | otherwise = mempty

rememberStage :: Linenumber -> BaseImage -> Acc -> Acc
rememberStage line from@BaseImage {image = Image _ als} (Acc _ stages o) = Acc from (Map.insert als line stages) o
rememberStage line from@BaseImage {image = Image _ als} Empty = Acc from (Map.singleton als line) Map.empty

rememberLine :: Linenumber -> Acc -> Acc
rememberLine line (Acc from stages o) = Acc from stages (Map.insert line from o)
rememberLine _ Empty = Empty

forgotToCleanup :: Shell.ParsedShell -> Bool
forgotToCleanup args
  | not hasUpdate = False
  | otherwise = not hasCleanup
  where
    hasCleanup =
      any (Shell.cmdHasArgs "rm" ["-rf", "/var/lib/apt/lists/*"]) (Shell.presentCommands args)

    hasUpdate = any (Shell.cmdHasArgs "apt-get" ["update"]) (Shell.presentCommands args)
