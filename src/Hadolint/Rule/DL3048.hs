module Hadolint.Rule.DL3048 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax


rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL3048"
    severity = DLStyleC
    message = "Invalid label key."
    check (Label pairs) = hasNoInvalidKey pairs
    check _ = True

    hasNoInvalidKey prs = null $ [(l, v) | (l, v) <- prs,
                                          Text.take 1 l `notElem`
                                              fmap Text.singleton ['a'..'z'] ||
                                          Text.takeEnd 1 l `notElem`
                                              fmap Text.singleton (['a'..'z'] ++ ['0'..'9']) ||
                                          containsIllegalChar l ||
                                          hasReservedNamespace l ||
                                          hasConsecutiveSeparators l]
{-# INLINEABLE rule #-}

containsIllegalChar :: Text.Text -> Bool
containsIllegalChar = Text.any (`notElem` validChars)

hasReservedNamespace :: Text.Text -> Bool
hasReservedNamespace l = "com.docker." `Text.isPrefixOf` l
  || "io.docker." `Text.isPrefixOf` l
  || "org.dockerproject." `Text.isPrefixOf` l

hasConsecutiveSeparators :: Text.Text -> Bool
hasConsecutiveSeparators l = ".." `Text.isInfixOf` l || "--" `Text.isInfixOf` l

validChars :: String
validChars = ['0'..'9'] ++ ['a' .. 'z'] ++ ['.', '-']
