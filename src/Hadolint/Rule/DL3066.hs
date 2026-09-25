module Hadolint.Rule.DL3066 (rule) where

import qualified Data.Char as Char
import qualified Data.IntMap.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax


data Acc
  = Acc
      { stageLine :: Linenumber,
        lines :: Map.IntMap Linenumber,
        args :: Set.Set Text.Text
      }
  | Empty
  deriving (Show)


rule :: Rule args
rule = veryCustomRule check (emptyState Empty) mark
  where
    code = "DL3066"
    severity = DLInfoC
    message = "Non-numeric user-id may not be resolvable by host system"

    check line st (From _) = st |> modify forgetStage |> modify (rememberStage line)
    check line st (User u)
      | Text.all Char.isDigit $ getUid u = st |> modify forgetStage
      | uidIsDefinedArg (state st) u = st |> modify forgetStage
      | otherwise = st |> modify (rememberLine line)
    check _ st (Arg arg _) = st |> modify (registerArg arg)
    check _ st _ = st

    mark (State fails (Acc _ lines _)) = Map.foldl' (Seq.|>) fails (fmap fail lines)
    mark st = failures st

    fail line = CheckFailure {..}
{-# INLINEABLE rule #-}

rememberStage :: Linenumber -> Acc -> Acc
rememberStage line Empty = Acc line Map.empty Set.empty
rememberStage line (Acc _ lines args) = Acc line lines args

forgetStage :: Acc -> Acc
forgetStage Empty = Empty
forgetStage (Acc stage lines args) = Acc stage (lines |> Map.delete stage) args

rememberLine :: Linenumber -> Acc -> Acc
rememberLine line Empty = Acc 0 (Map.singleton 0 line) Set.empty
rememberLine line (Acc stage lines args) = Acc stage (lines |> Map.insert stage line) args

getUid :: Text.Text -> Text.Text
getUid t
  | ":" `Text.isInfixOf` t = u ( Text.splitOn ":" t )
  | otherwise = t
  where
    u [] = ""
    u (h:_) = h

uidIsDefinedArg :: Acc -> Text.Text -> Bool
uidIsDefinedArg Empty _ = False
uidIsDefinedArg (Acc _ _ args) u = any (`varInUid` u) args
  where
    varInUid :: Text.Text -> Text.Text -> Bool
    varInUid var uid =
      ( Text.pack "${" <> var <> Text.pack "}" ) `Text.isInfixOf` uid
        || ( Text.pack "$" <> var ) `Text.isInfixOf` uid

registerArg :: Text.Text -> Acc -> Acc
registerArg arg Empty = Acc 0 Map.empty (Set.singleton arg)
registerArg arg (Acc line lines args) = Acc line lines (Set.insert arg args)
