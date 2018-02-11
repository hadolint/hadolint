{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Formatter.Checkstyle
    ( printResult
    , formatResult
    ) where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.DList (toList)
import Data.List (groupBy)
import Data.Monoid ((<>), mconcat)
import Hadolint.Formatter.Format
import Hadolint.Rules (Metadata(..), RuleCheck(..))
import ShellCheck.Interface
import Text.Parsec (ParseError)
import Text.Parsec.Error (ParseError, errorPos)
import Text.Parsec.Pos

data CheckStyle = CheckStyle
    { file :: String
    , line :: Int
    , column :: Int
    , impact :: String
    , msg :: String
    , source :: String
    }

errorToCheckStyle :: ParseError -> CheckStyle
errorToCheckStyle err =
    CheckStyle
    { file = sourceName pos
    , line = sourceLine pos
    , column = sourceColumn pos
    , impact = severityText ErrorC
    , msg = stripNewlines (formatErrorReason err)
    , source = "DL1000"
    }
  where
    pos = errorPos err

ruleToCheckStyle :: RuleCheck -> CheckStyle
ruleToCheckStyle RuleCheck {..} =
    CheckStyle
    { file = filename
    , line = linenumber
    , column = 1
    , impact = severityText (severity metadata)
    , msg = message metadata
    , source = code metadata
    }

toXml :: [CheckStyle] -> Builder.Builder
toXml checks = wrap fileName (foldMap convert checks)
  where
    wrap name innerNode = "<file " <> attr "name" name <> ">" <> innerNode <> "</file>"
    convert CheckStyle {..} =
        "<error " <> -- Beging the node construction
        attr "line" (show line) <>
        attr "column" (show column) <>
        attr "severity" impact <>
        attr "message" msg <>
        attr "source" source <>
        "/>"
    fileName = file (head checks)

attr name value = Builder.string8 name <> "='" <> Builder.string8 (escape value) <> "' "

escape = concatMap doEscape
  where
    doEscape c =
        if isOk c
            then [c]
            else "&#" ++ show (ord c) ++ ";"
    isOk x = any ($x) [isAsciiUpper, isAsciiLower, isDigit, (`elem` [' ', '.', '/'])]

formatResult :: Result -> Builder.Builder
formatResult (Result errors checks) =
    "<?xml version='1.0' encoding='UTF-8'?><checkstyle version='4.3'>" <> xmlBody <> "</checkstyle>"
  where
    xmlBody = mconcat xmlChunks
    xmlChunks = fmap toXml (groupBy sameFileName flatten)
    flatten = toList $ checkstyleErrors <> checkstyleChecks
    checkstyleErrors = fmap errorToCheckStyle errors
    checkstyleChecks = fmap ruleToCheckStyle checks
    sameFileName CheckStyle {file = f1} CheckStyle {file = f2} = f1 == f2

printResult :: Result -> IO ()
printResult result = B.putStr (Builder.toLazyByteString (formatResult result))
