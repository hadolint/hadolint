module Hadolint.Rule.DL3066 (rule) where

import qualified Data.Char as Char
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL3066"
    severity = DLInfoC
    message = "Non-numeric user-id may not be resolvable by host system"

    check (User u) = Text.all Char.isDigit $ getUid u
    check _ = True
{-# INLINEABLE rule #-}

getUid :: Text.Text -> Text.Text
getUid t
  | ":" `Text.isInfixOf` t = u ( Text.splitOn ":" t )
  | otherwise = t
  where
    u [] = ""
    u (h:_) = h
