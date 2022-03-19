module Hadolint.Rule.Shellcheck (rule) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell
import Hadolint.Pragma (parseShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax
import qualified ShellCheck.Interface


data Acc
  = Acc
      { opts :: Shell.ShellOpts,
        defaultOpts :: Shell.ShellOpts
      }
  | Empty


rule :: Rule Shell.ParsedShell
rule = scrule <> onbuild scrule
{-# INLINEABLE rule #-}

scrule :: Rule Shell.ParsedShell
scrule = customRule check (emptyState Empty)
  where
    check _ st (From _) = st |> modify newStage
    check _ st (Arg name _) = st |> modify (addVars [name])
    check _ st (Env pairs) = st |> modify (addVars (map fst pairs))
    check _ st (Shell args) =
      st |> modify (setShell (foldArguments Shell.original args))
    check _ st (Comment com) =
      case Hadolint.Pragma.parseShell com of
        Just sh -> st |> modify (shellPragma sh)
        _ -> st
    check line st (Run (RunArgs args _)) = getFailures (state st) |> foldr addFail st
      where
        getFailures Empty = foldArguments (runShellCheck Shell.defaultShellOpts) args
        getFailures s = foldArguments (runShellCheck (opts s)) args
        runShellCheck options script =
          Set.fromList
            [ toFailure line c
              | c <- Shell.shellcheck options script
            ]
    check _ st _ = st
{-# INLINEABLE scrule #-}

newStage :: Acc -> Acc
newStage Empty =
  Acc
    { opts = Shell.defaultShellOpts,
      defaultOpts = Shell.defaultShellOpts
    }
newStage Acc {..} =
  Acc
    { opts = defaultOpts,
      defaultOpts
    }

addVars :: [Text.Text] -> Acc -> Acc
addVars vars Empty =
  Acc
    { opts =
        Shell.ShellOpts
          { shellName = Shell.shellName Shell.defaultShellOpts,
            envVars = Shell.envVars Shell.defaultShellOpts <> Set.fromList vars
          },
      defaultOpts = Shell.defaultShellOpts
    }
addVars vars Acc {..} =
  Acc
    { opts =
        Shell.ShellOpts
          { shellName = Shell.shellName opts,
            envVars = Shell.envVars opts <> Set.fromList vars
          },
      defaultOpts
    }

setShell :: Text.Text -> Acc -> Acc
setShell sh Empty =
  Acc
    { opts =
        Shell.ShellOpts
          { shellName = sh,
            envVars = Shell.envVars Shell.defaultShellOpts
          },
      defaultOpts = Shell.defaultShellOpts
    }
setShell sh Acc {..} =
  Acc
    { opts =
        Shell.ShellOpts
          { shellName = sh,
            envVars = Shell.envVars opts
          },
      defaultOpts
    }

shellPragma :: Text.Text -> Acc -> Acc
shellPragma sh Empty =
  Acc
    { opts =
        Shell.ShellOpts
          { shellName = sh,
            envVars = Shell.envVars Shell.defaultShellOpts
          },
      defaultOpts =
        Shell.ShellOpts
          { shellName = sh,
            envVars = Shell.envVars Shell.defaultShellOpts
          }
    }
shellPragma sh Acc {..} =
  Acc
    { opts =
        Shell.ShellOpts
          { shellName = sh,
            envVars = Shell.envVars opts
          },
      defaultOpts
    }

-- | Converts ShellCheck errors into our own errors type
toFailure :: Linenumber ->
  ShellCheck.Interface.PositionedComment ->
  CheckFailure
toFailure line c =
  CheckFailure
    { code = RuleCode $ Text.pack ("SC" ++ show (code c)),
      severity = getDLSeverity $ severity c,
      message = Text.pack (message c),
      line = line
    }
  where
    severity pc =
      ShellCheck.Interface.cSeverity $ ShellCheck.Interface.pcComment pc
    code pc = ShellCheck.Interface.cCode $ ShellCheck.Interface.pcComment pc
    message pc =
      ShellCheck.Interface.cMessage $ ShellCheck.Interface.pcComment pc

getDLSeverity :: ShellCheck.Interface.Severity -> DLSeverity
getDLSeverity s =
  case s of
    ShellCheck.Interface.WarningC -> DLWarningC
    ShellCheck.Interface.InfoC -> DLInfoC
    ShellCheck.Interface.StyleC -> DLStyleC
    _ -> DLErrorC
