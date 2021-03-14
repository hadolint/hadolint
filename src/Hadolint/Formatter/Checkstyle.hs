module Hadolint.Formatter.Checkstyle
  ( printResults,
    formatResult,
  )
where

import qualified Control.Foldl as Foldl
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8Builder)
import Hadolint.Formatter.Format
  ( Result (..),
    errorBundlePretty,
    errorPosition,
    severityText,
  )
import Hadolint.Rule (CheckFailure (..), DLSeverity (..), RuleCode (..))
import System.IO (stdout)
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
  ( ParseErrorBundle,
    ShowErrorComponent,
  )
import Text.Megaparsec.Pos (sourceColumn, sourceLine, unPos)
import Text.Megaparsec.Stream (VisualStream)

data CheckStyle = CheckStyle
  { line :: Int,
    column :: Int,
    impact :: Text.Text,
    msg :: Text.Text,
    source :: Text.Text
  }

errorToCheckStyle :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => ParseErrorBundle s e -> CheckStyle
errorToCheckStyle err =
  CheckStyle
    { line = unPos (sourceLine pos),
      column = unPos (sourceColumn pos),
      impact = severityText DLErrorC,
      msg = Text.pack (errorBundlePretty err),
      source = "DL1000"
    }
  where
    pos = errorPosition err

ruleToCheckStyle :: CheckFailure -> CheckStyle
ruleToCheckStyle CheckFailure {..} =
  CheckStyle
    { line = line,
      column = 1,
      impact = severityText severity,
      msg = message,
      source = unRuleCode code
    }

toXml :: CheckStyle -> Builder.Builder
toXml CheckStyle {..} =
  "<error "
    <> attr "line" (Builder.intDec line)
    <> attr "column" (Builder.intDec column)
    <> attr "severity" (encode impact)
    <> attr "message" (encode msg)
    <> attr "source" (encode source)
    <> "/>"

encode :: Text.Text -> Builder.Builder
encode = encodeUtf8Builder . escape

attr :: Text.Text -> Builder.Builder -> Builder.Builder
attr name value = encodeUtf8Builder name <> "='" <> value <> "' "

escape :: Text.Text -> Text.Text
escape = Text.concatMap doEscape
  where
    doEscape c =
      if isOk c
        then Text.singleton c
        else "&#" <> Text.pack (show (ord c)) <> ";"
    isOk x = any (\check -> check x) [isAsciiUpper, isAsciiLower, isDigit, (`elem` [' ', '.', '/'])]

formatResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Builder.Builder
formatResult (Result filename errors checks) = header <> xmlBody <> footer
  where
    xmlBody = Foldl.fold (Foldl.premap toXml Foldl.mconcat) issues

    issues = checkstyleErrors <> checkstyleChecks
    checkstyleErrors = fmap errorToCheckStyle errors
    checkstyleChecks = fmap ruleToCheckStyle checks

    isEmpty = null checks && null errors
    header =
      if isEmpty
        then ""
        else "<file " <> attr "name" (encode filename) <> ">"
    footer = if isEmpty then "" else "</file>"

printResults ::
  (Foldable f, VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  f (Result s e) ->
  IO ()
printResults results = do
  B.putStr header
  mapM_ put results
  B.putStr footer
  where
    header = "<?xml version='1.0' encoding='UTF-8'?><checkstyle version='4.3'>"
    footer = "</checkstyle>"
    put result = Builder.hPutBuilder stdout (formatResult result)
