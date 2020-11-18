{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hadolint.Formatter.Checkstyle
  ( printResult,
    formatResult,
  )
where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.Foldable (toList)
import Data.List (groupBy)
import qualified Data.Text as Text
import Hadolint.Formatter.Format
import Hadolint.Rules (Metadata (..), RuleCheck (..))
import ShellCheck.Interface
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos (sourceColumn, sourceLine, sourceName, unPos)
import Text.Megaparsec.Stream (VisualStream)

data CheckStyle = CheckStyle
  { file :: String,
    line :: Int,
    column :: Int,
    impact :: String,
    msg :: String,
    source :: String
  }

errorToCheckStyle :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => ParseErrorBundle s e -> CheckStyle
errorToCheckStyle err =
  CheckStyle
    { file = sourceName pos,
      line = unPos (sourceLine pos),
      column = unPos (sourceColumn pos),
      impact = severityText (Just ErrorC),
      msg = errorBundlePretty err,
      source = "DL1000"
    }
  where
    pos = errorPosition err

ruleToCheckStyle :: RuleCheck -> CheckStyle
ruleToCheckStyle RuleCheck {..} =
  CheckStyle
    { file = Text.unpack filename,
      line = linenumber,
      column = 1,
      impact = severityText (severity metadata),
      msg = Text.unpack (message metadata),
      source = Text.unpack (code metadata)
    }

toXml :: [CheckStyle] -> Builder.Builder
toXml checks = wrap fileName (foldMap convert checks)
  where
    wrap name innerNode = "<file " <> attr "name" name <> ">" <> innerNode <> "</file>"
    convert CheckStyle {..} =
      "<error "
        <> attr "line" (show line) -- Beging the node construction
        <> attr "column" (show column)
        <> attr "severity" impact
        <> attr "message" msg
        <> attr "source" source
        <> "/>"
    fileName =
      case checks of
        [] -> ""
        h : _ -> file h

attr :: String -> String -> Builder.Builder
attr name value = Builder.string8 name <> "='" <> Builder.string8 (escape value) <> "' "

escape :: String -> String
escape = concatMap doEscape
  where
    doEscape c =
      if isOk c
        then [c]
        else "&#" ++ show (ord c) ++ ";"
    isOk x = any (\check -> check x) [isAsciiUpper, isAsciiLower, isDigit, (`elem` [' ', '.', '/'])]

formatResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Builder.Builder
formatResult (Result errors checks) =
  "<?xml version='1.0' encoding='UTF-8'?><checkstyle version='4.3'>" <> xmlBody <> "</checkstyle>"
  where
    xmlBody = mconcat xmlChunks
    xmlChunks = fmap toXml (groupBy sameFileName flatten)
    flatten = toList $ checkstyleErrors <> checkstyleChecks
    checkstyleErrors = fmap errorToCheckStyle errors
    checkstyleChecks = fmap ruleToCheckStyle checks
    sameFileName CheckStyle {file = f1} CheckStyle {file = f2} = f1 == f2

printResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> IO ()
printResult result = B.putStr (Builder.toLazyByteString (formatResult result))
