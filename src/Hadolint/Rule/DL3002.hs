module Hadolint.Rule.DL3002 (rule) where

import Data.Char (isAlpha, isAlphaNum, isAscii)
import qualified Data.IntMap.Strict as Map
import qualified Data.Map.Strict as EnvMap
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax (Instruction (..), Linenumber, Pairs)

type StageLine = Linenumber

type UserLine = Linenumber

type Env = EnvMap.Map Text.Text Text.Text

data Acc
  = Acc StageLine (Map.IntMap UserLine) Env
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
      | isRoot (expandVars (envOf (state st)) (dropQuotes user)) =
          st |> modify (rememberLine line)
      | otherwise = st |> modify forgetStage
    check _ st (Env pairs) = st |> modify (registerEnvs pairs)
    check _ st _ = st

    isRoot user =
      Text.isPrefixOf "root:" user
        || Text.isPrefixOf "0:" user
        || user == "root"
        || user == "0"

    markFailures (State fails (Acc _ users _)) =
      Map.foldl' (Seq.|>) fails (fmap makeFail users)
    markFailures st = failures st
    makeFail line = CheckFailure {..}
{-# INLINEABLE rule #-}

rememberStage :: StageLine -> Acc -> Acc
rememberStage from (Acc _ users _) = Acc from users EnvMap.empty
rememberStage from Empty = Acc from Map.empty EnvMap.empty

forgetStage :: Acc -> Acc
forgetStage (Acc from users env) = Acc from (users |> Map.delete from) env
forgetStage Empty = Empty

rememberLine :: StageLine -> Acc -> Acc
rememberLine line (Acc from users env) =
  Acc from (users |> Map.insert from line) env
rememberLine _ Empty = Empty

registerEnvs :: Pairs -> Acc -> Acc
registerEnvs _ Empty = Empty
registerEnvs pairs (Acc from users env) =
  Acc from users (foldl' insertOne env pairs)
  where
    insertOne m (key, value) =
      EnvMap.insert key (expandVars m (dropQuotes value)) m

envOf :: Acc -> Env
envOf (Acc _ _ env) = env
envOf Empty = EnvMap.empty

-- | Replace `$name` and `${name}` from the stage's ENV instructions.
-- `$$` stays a literal `$`. `${name:-default}` and other modified forms are
-- left unchanged, and a name that was never set is left unchanged.
expandVars :: Env -> Text.Text -> Text.Text
expandVars env = go
  where
    go text
      | Text.null text = text
      | otherwise =
          case Text.breakOn "$" text of
            (before, rest)
              | Text.null rest -> before
              | otherwise ->
                  let (replacement, after) = takeVar (Text.drop 1 rest)
                   in before <> replacement <> go after

    takeVar rest
      | Text.isPrefixOf "$" rest = ("$", Text.drop 1 rest)
      | Text.isPrefixOf "{" rest =
          case Text.breakOn "}" (Text.drop 1 rest) of
            (name, after)
              | not (Text.null after),
                validName name,
                Just value <- EnvMap.lookup name env ->
                  (value, Text.drop 1 after)
            _ -> ("$", rest)
      | otherwise =
          let (name, after) = Text.span isNameChar rest
           in if validName name
                then case EnvMap.lookup name env of
                  Just value -> (value, after)
                  Nothing -> ("$" <> name, after)
                else ("$", rest)

validName :: Text.Text -> Bool
validName name =
  case Text.uncons name of
    Just (first, rest) ->
      isNameStart first && Text.all isNameChar rest
    Nothing -> False

isNameStart :: Char -> Bool
isNameStart c = c == '_' || isAscii c && isAlpha c

isNameChar :: Char -> Bool
isNameChar c = c == '_' || isAscii c && isAlphaNum c
