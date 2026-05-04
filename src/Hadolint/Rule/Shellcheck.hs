module Hadolint.Rule.Shellcheck (rule) where

import qualified Data.Map as Map
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
        defaultOpts :: Shell.ShellOpts,
        stageIdx :: Int,
        stageOpts :: Map.Map Int Shell.ShellOpts,
        stages :: Map.Map Int Text.Text
      }
  | Empty


rule :: Rule Shell.ParsedShell
rule = scrule <> onbuild scrule
{-# INLINEABLE rule #-}

scrule :: Rule Shell.ParsedShell
scrule = customRule check (emptyState Empty)
  where
    check _ st (From bi) = st |> modify (newStage bi)
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

newStage :: BaseImage -> Acc -> Acc
newStage BaseImage {..} Empty =
  Acc
    { opts = Shell.defaultShellOpts,
      defaultOpts = Shell.defaultShellOpts,
      stageIdx = 0,
      stageOpts = Map.singleton 0 Shell.defaultShellOpts,
      stages = fromAlias alias
    }
  where
    fromAlias Nothing = Map.empty
    fromAlias (Just a) = Map.singleton 0 (unImageAlias a)
newStage BaseImage {..} Acc {..} =
  if Map.null (Map.filter (== imageName image) stages)
  then
    Acc
      { opts = defaultOpts,
        defaultOpts,
        stageIdx = stageIdx + 1,
        stageOpts = Map.insert (stageIdx + 1) defaultOpts stageOpts,
        stages = fromAlias (stageIdx + 1) alias
      }
  else do
  let idx = getIdx getList
      theOpts = toOpts ( Map.lookup idx stageOpts )
   in
      Acc
        { opts = theOpts,
          defaultOpts,
          stageIdx = stageIdx + 1,
          stageOpts = Map.insert (stageIdx + 1) theOpts stageOpts,
          stages = fromAlias (stageIdx + 1) alias
        }
  where
    fromAlias _ Nothing = stages
    fromAlias idx (Just a) = Map.insert idx (unImageAlias a) stages

    getIdx [] = 0
    getIdx [(k, _)] = k
    getIdx ((k, _):_:_) = k

    getList = Map.toList (Map.filter (== imageName image) stages)

    toOpts Nothing = defaultOpts
    toOpts (Just o) = o

addVars :: [Text.Text] -> Acc -> Acc
addVars vars Empty = do
  let opts =
        Shell.ShellOpts
          { shellName = Shell.shellName Shell.defaultShellOpts,
            envVars = Shell.envVars Shell.defaultShellOpts <> Set.fromList vars
          }
   in
    Acc
      {
        opts,
        defaultOpts = Shell.defaultShellOpts,
        stageIdx = 0,
        stageOpts = Map.singleton 0 opts,
        stages = Map.empty
      }
addVars vars Acc {..} = do
  let newOpts =
        Shell.ShellOpts
          { shellName = Shell.shellName Shell.defaultShellOpts,
            envVars = Shell.envVars opts <> Set.fromList vars
          }
   in
    Acc
      { opts = newOpts,
        defaultOpts,
        stageIdx,
        stageOpts = Map.update (\_ -> Just newOpts) stageIdx stageOpts,
        stages
      }

setShell :: Text.Text -> Acc -> Acc
setShell sh Empty = do
  let opts =
        Shell.ShellOpts
          { shellName = sh,
            envVars = Shell.envVars Shell.defaultShellOpts
          }
   in
    Acc
      { opts,
        defaultOpts = Shell.defaultShellOpts,
        stageIdx = 0,
        stageOpts = Map.singleton 0 opts,
        stages = Map.empty
      }
setShell sh Acc {..} = do
  let newOpts =
        Shell.ShellOpts
          { shellName = sh,
            envVars = Shell.envVars opts
          }
   in
    Acc
      { opts = newOpts,
        defaultOpts,
        stageIdx,
        stageOpts = Map.update (\_ -> Just newOpts) stageIdx stageOpts,
        stages
      }

shellPragma :: Text.Text -> Acc -> Acc
shellPragma sh Empty = do
  let newOpts =
        Shell.ShellOpts
          { shellName = sh,
            envVars = Shell.envVars Shell.defaultShellOpts
          }
   in
    Acc
      { opts = newOpts,
        defaultOpts = newOpts,
        stageIdx = 0,
        stageOpts = Map.singleton 0 newOpts,
        stages = Map.empty
      }
shellPragma sh Acc {..} = do
  let newOpts =
        Shell.ShellOpts
          { shellName = sh,
            envVars = Shell.envVars opts
          }
   in
    Acc
      { opts = newOpts,
        defaultOpts,
        stageIdx,
        stageOpts = Map.update (\_ -> Just newOpts) stageIdx stageOpts,
        stages
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
