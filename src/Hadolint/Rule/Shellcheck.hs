module Hadolint.Rule.Shellcheck (rule) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax
import qualified ShellCheck.Interface

rule :: Rule ParsedShell
rule = customRule check (emptyState Shell.defaultShellOpts)
  where
    check _ st (From _) = st |> replaceWith Shell.defaultShellOpts -- Reset the state
    check _ st (Arg name _) = st |> modify (Shell.addVars [name])
    check _ st (Env pairs) = st |> modify (Shell.addVars (map fst pairs))
    check _ st (Shell args) = st |> modify (Shell.setShell (foldArguments Shell.original args))
    check line st (Run (RunArgs args _)) = getFailures |> foldr addFail st
      where
        getFailures = foldArguments (runShellCheck (state st)) args
        runShellCheck opts script = Set.fromList [toFailure line c | c <- Shell.shellcheck opts script]
    check _ st _ = st
{-# INLINEABLE rule #-}

-- | Converts ShellCheck errors into our own errors type
toFailure :: Linenumber -> ShellCheck.Interface.PositionedComment -> CheckFailure
toFailure line c =
  CheckFailure
    { code = RuleCode $ Text.pack ("SC" ++ show (code c)),
      severity = getDLSeverity $ severity c,
      message = Text.pack (message c),
      line = line
    }
  where
    getDLSeverity :: ShellCheck.Interface.Severity -> DLSeverity
    getDLSeverity s =
      case s of
        ShellCheck.Interface.WarningC -> DLWarningC
        ShellCheck.Interface.InfoC -> DLInfoC
        ShellCheck.Interface.StyleC -> DLStyleC
        _ -> DLErrorC
    severity pc = ShellCheck.Interface.cSeverity $ ShellCheck.Interface.pcComment pc
    code pc = ShellCheck.Interface.cCode $ ShellCheck.Interface.pcComment pc
    message pc = ShellCheck.Interface.cMessage $ ShellCheck.Interface.pcComment pc
