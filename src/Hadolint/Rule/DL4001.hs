module Hadolint.Rule.DL4001 (rule) where

import qualified Data.IntSet as Set
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax (Instruction (..), RunArgs (..))

rule :: Rule Shell.ParsedShell
rule = customRule check (emptyState Set.empty)
  where
    code = "DL4001"
    severity = DLWarningC
    message = "Either use Wget or Curl but not both"

    check line st (Run (RunArgs args _)) =
      let newArgs = foldArguments extractCommands args
          newState = st |> modify (Set.union newArgs)
       in if Set.size newArgs > 0 && Set.size (state newState) >= 2
            then newState |> addFail (CheckFailure {..})
            else newState
    -- Reset the state for each stage
    check _ st From {} = st |> replaceWith Set.empty
    check _ st _ = st
{-# INLINEABLE rule #-}

extractCommands :: Shell.ParsedShell -> Set.IntSet
extractCommands args =
  Set.fromList
    [ if w == "curl" then 0 else 1
      | w <- Shell.findCommandNames args,
        w == "curl" || w == "wget"
    ]
