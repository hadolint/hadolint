module Hadolint.Rule.DL3063 (rule) where

import qualified Data.Sequence as Seq
import Hadolint.Rule
import Language.Docker.Syntax (Instruction (..), Linenumber)

data Acc
  = Acc { hasUser :: Bool, reportLine :: Linenumber }
  | Empty
  deriving (Eq, Show)

rule :: Rule args
rule = veryCustomRule check (emptyState Empty) markFailures
  where
    code = "DL3063"
    severity = DLIgnoreC
    message = "A USER instruction should be specified"

    check line st (From _) = st |> modify (setReportLine (line + 1))
    check _ st (User _) = st |> modify setHasUser
    check _ st _ = st

    setReportLine :: Linenumber -> Acc -> Acc
    setReportLine line Empty = Acc False line
    setReportLine line (Acc False _) = Acc False line
    setReportLine _ st@(Acc True _) = st

    setHasUser :: Acc -> Acc
    setHasUser (Acc _ line) = Acc True line
    setHasUser Empty = Acc True 1

    markFailures (State fails (Acc False line)) = fails Seq.|> makeFail line
    markFailures st = failures st

    makeFail line = CheckFailure {..}
{-# INLINEABLE rule #-}
