module Hadolint.Rule.DL3066 (rule) where

import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax


data Acc
  = Acc { args :: Set.Set Text.Text }
  | Empty
  deriving (Show)


rule :: Rule args
rule = customRule check (emptyState Empty)
  where
    code = "DL3066"
    severity = DLInfoC
    message = "Non-numeric user-id may not be resolvable by host system"

    check line st (User u)
      | Text.all Char.isDigit $ getUid u = st
      | uidIsDefinedArg (state st) u = st
      | otherwise = st |> addFail CheckFailure {..}
    check _ st (Arg arg _) = st |> modify (registerArg arg)
    check _ st _ = st
{-# INLINEABLE rule #-}

getUid :: Text.Text -> Text.Text
getUid t
  | ":" `Text.isInfixOf` t = u ( Text.splitOn ":" t )
  | otherwise = t
  where
    u [] = ""
    u (h:_) = h

uidIsDefinedArg :: Acc -> Text.Text -> Bool
uidIsDefinedArg Empty _ = False
uidIsDefinedArg (Acc args) u = any (`varInUid` u) args
  where
    varInUid :: Text.Text -> Text.Text -> Bool
    varInUid var uid =
      ( Text.pack "${" <> var <> Text.pack "}" ) `Text.isInfixOf` uid
        || ( Text.pack "$" <> var ) `Text.isInfixOf` uid

registerArg :: Text.Text -> Acc -> Acc
registerArg arg Empty =
  Acc { args = Set.singleton arg }
registerArg arg (Acc args) =
  Acc { args = Set.insert arg args }

