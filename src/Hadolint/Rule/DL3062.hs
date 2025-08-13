module Hadolint.Rule.DL3062 (rule) where

import qualified Data.IntMap.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax (Instruction (..), Linenumber)

type StageLine = Linenumber

data Acc
  = Acc (Map.IntMap UserLine)
  | Empty
  deriving (Show)

type UserLine = Linenumber

rule :: Rule args
rule = veryCustomRule check (emptyState Empty) markFailures
  where
    code = "DL3062"
    severity = DLWarningC
    message = "A USER should be specified"

    check :: Linenumber -> State Acc -> Instruction -> State Acc
    check line st (User user) = st |> modify (rememberLine line)
    check _ st _ = st

    markFailures :: State Acc -> Seq.Seq CheckFailure
    markFailures (State fails Empty) = fails Seq.|> makeFail 0
    markFailures st = failures st

    makeFail :: Linenumber -> CheckFailure
    makeFail line = CheckFailure {..}


rememberLine :: UserLine -> Acc -> Acc
rememberLine line (Acc m) = Acc (m |> Map.insert line line)
rememberLine line Empty = Acc (Map.singleton line line)
