module Hadolint.Rule.DL3041 (rule) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax
import Data.Char (isDigit, isAsciiUpper, isAsciiLower)

data Acc
  = Acc { envs :: Set.Set Text.Text,
          args :: Set.Set Text.Text
        }
  | Empty
  deriving (Show)

rule :: Rule Shell.ParsedShell
rule = dl3041 <> onbuild dl3041
{-# INLINEABLE rule #-}

dl3041 :: Rule Shell.ParsedShell
dl3041 = customRule check (emptyState Empty)
  where
    code = "DL3041"
    severity = DLWarningC
    message = "Specify version with `dnf install -y <package>-<version>`."

    check line st (Run (RunArgs a _))
      | foldArguments (all ( packageVersionFixed ( state st ) ) . dnfPackages) a
          && foldArguments (all moduleVersionFixed . dnfModules) a = st
      | otherwise = st |> addFail CheckFailure {..}
    check _ st (Env pairs) = st |> modify (registerEnvs pairs)
    check _ st (Arg arg _) = st |> modify (registerArg arg)
    check _ st (From _) = st |> modify (resetEnv)
    check _ st _ = st
{-# INLINEABLE dl3041 #-}

dnfCmds :: [Text.Text]
dnfCmds = ["dnf", "microdnf"]

dnfPackages :: Shell.ParsedShell -> [Text.Text]
dnfPackages args =
    [ arg
      | cmd <- Shell.presentCommands args,
        not (Shell.cmdsHaveArgs dnfCmds ["module", "group"] cmd),
        arg <- installFilter cmd
    ]

packageVersionFixed :: Acc -> Text.Text -> Bool
packageVersionFixed acc package
  | length parts <= 1 = False  -- No dashes, definitively no version
  | ".rpm" `Text.isSuffixOf` package = True  -- rpm files always have a version
  | "$" `Text.isInfixOf` package = envDefined acc package
  | otherwise = isVersionLike $ drop 1 parts
  where
    parts = Text.splitOn "-" package

envDefined :: Acc -> Text.Text -> Bool
envDefined Empty _ = False
envDefined (Acc envs args) package =
  any (\v -> varInText v package) envs
    || any (`varInText` package) args

varInText :: Text.Text -> Text.Text -> Bool
varInText var txt =
  ( Text.pack "${" <> var <> Text.pack "}" ) `Text.isInfixOf` txt

isVersionLike :: [Text.Text] -> Bool
isVersionLike parts =
  case parts of
    [] -> False  -- No parts after splitting by hyphen
    _ -> all partIsValid parts && any partStartsWithDigit parts
  where
    partIsValid part = Text.all isVersionChar part
    partStartsWithDigit part = case Text.uncons part of
                                 Just (c, _) -> isDigit c
                                 Nothing -> False -- Empty Text

isVersionChar :: Char -> Bool
isVersionChar c =
  isDigit c
    || isAsciiUpper c
    || isAsciiLower c
    || c `elem` ['.', '~', '^', '_', ':', '+']

dnfModules :: Shell.ParsedShell -> [Text.Text]
dnfModules args =
  [ arg
    | cmd <- Shell.presentCommands args,
      Shell.cmdsHaveArgs dnfCmds ["module"] cmd,
      arg <- installFilter cmd
  ]

moduleVersionFixed :: Text.Text -> Bool
moduleVersionFixed = Text.isInfixOf ":"

installFilter :: Shell.Command -> [Text.Text]
installFilter cmd =
  [ arg
    | Shell.cmdsHaveArgs dnfCmds ["install"] cmd,
      arg <- Shell.getArgsNoFlags cmd,
      arg /= "install",
      arg /= "module"
  ]

registerEnvs :: Pairs -> Acc -> Acc
registerEnvs pairs Empty =
  Acc
    { envs = Set.fromList (map fst pairs),
      args = Set.empty
    }
registerEnvs pairs (Acc envs args) =
  Acc
    { envs = Set.union (Set.fromList (map fst pairs)) envs,
      args
    }

registerArg :: Text.Text -> Acc -> Acc
registerArg arg Empty =
  Acc
    { envs = Set.empty,
      args = Set.singleton arg
    }
registerArg arg (Acc envs args) =
  Acc
    { envs,
      args = Set.insert arg args
    }

resetEnv :: Acc -> Acc
resetEnv Empty = Empty
resetEnv (Acc _ args) =
  Acc
    { envs = Set.empty,
      args = args
    }
