module Hadolint.Rule.DL3022 (rule) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import Hadolint.Rule
import Language.Docker.Syntax

data Acc
  = Acc { count :: Int, names :: Set.Set Text.Text }
  | Empty
  deriving (Eq)

rule :: Rule args
rule = customRule check (emptyState Empty)
  where
    code = "DL3022"
    severity = DLWarningC
    message = "`COPY --from` should reference a previously defined `FROM` alias"

    check _ st (From BaseImage {alias = Just (ImageAlias als)}) = st |> modify (incAndAddName als)
    check _ st (From BaseImage {}) = st |> modify incCount
    check line st (Copy (CopyArgs _ _) (CopyFlags _ _ _ (CopySource s)))
      | ":" `Text.isInfixOf` dropQuotes s = st
      | isMember s (state st) = st
      | otherwise = case Read.decimal s of
                      Right (v, _) | v < nameCount (state st) -> st
                      _ -> st |> addFail CheckFailure {..}
    check _ st _ = st
{-# INLINEABLE rule #-}

incAndAddName :: Text.Text -> Acc -> Acc
incAndAddName s Empty = Acc { count = 1, names = Set.singleton s }
incAndAddName s Acc { count, names } = Acc { count = count + 1, names = Set.insert s names }

incCount :: Acc -> Acc
incCount Empty = Acc { count = 1, names = Set.empty }
incCount Acc { count, names } = Acc { count = count + 1, names = names }

isMember :: Text.Text -> Acc -> Bool
isMember _ Empty = False
isMember s Acc { names } = Set.member s names

nameCount :: Acc -> Int
nameCount Empty = 0
nameCount Acc { count } = count
