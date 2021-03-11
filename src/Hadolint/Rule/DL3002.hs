module Hadolint.Rule.DL3002 (rule) where

import qualified Data.IntMap.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax (Instruction (..), Linenumber)

type StageLine = Linenumber

type UserLine = Linenumber

data Acc
  = Acc StageLine (Map.IntMap UserLine)
  | Empty

rule :: Rule args
rule = veryCustomRule check (emptyState Empty) markFailures
  where
    code = "DL3002"
    severity = DLWarningC
    message = "Last USER should not be root"

    check line st@(State _ acc) (From _) = st |> replaceWith (rememberStage acc line)
    check line st@(State _ acc) (User user)
      | isRoot user = st |> replaceWith (forgetStage acc)
      | otherwise = st |> replaceWith (rememberLine acc line)
    check _ st _ = st

    isRoot user =
      Text.isPrefixOf "root:" user || Text.isPrefixOf "0:" user || user == "root" || user == "0"

    markFailures (State fails (Acc _ st)) = Map.foldl' (Seq.|>) fails (fmap makeFail st)
    markFailures st = failures st
    makeFail line = CheckFailure {..}

rememberStage :: Acc -> StageLine -> Acc
rememberStage (Acc _ m) from = Acc from m
rememberStage Empty from = Acc from Map.empty

forgetStage :: Acc -> Acc
forgetStage (Acc from m) = Acc from (m |> Map.delete from)
forgetStage Empty = Empty

rememberLine :: Acc -> UserLine -> Acc
rememberLine (Acc from m) line = Acc from (m |> Map.insert from line)
rememberLine Empty _ = Empty
