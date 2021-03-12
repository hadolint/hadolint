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
  deriving (Show)

rule :: Rule args
rule = veryCustomRule check (emptyState Empty) markFailures
  where
    code = "DL3002"
    severity = DLWarningC
    message = "Last USER should not be root"

    check line st (From _) = st |> modify (rememberStage line)
    check line st (User user)
      | not (isRoot user) = st |> modify forgetStage
      | otherwise = st |> modify (rememberLine line)
    check _ st _ = st

    isRoot user =
      Text.isPrefixOf "root:" user || Text.isPrefixOf "0:" user || user == "root" || user == "0"

    markFailures (State fails (Acc _ st)) = Map.foldl' (Seq.|>) fails (fmap makeFail st)
    markFailures st = failures st
    makeFail line = CheckFailure {..}
{-# INLINEABLE rule #-}

rememberStage :: StageLine -> Acc -> Acc
rememberStage from (Acc _ m) = Acc from m
rememberStage from Empty = Acc from Map.empty

forgetStage :: Acc -> Acc
forgetStage (Acc from m) = Acc from (m |> Map.delete from)
forgetStage Empty = Empty

rememberLine :: StageLine -> Acc -> Acc
rememberLine line (Acc from m) = Acc from (m |> Map.insert from line)
rememberLine _ Empty = Empty
