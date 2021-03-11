{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Formatter.TTY
  ( printResults,
    formatCheck,
    formatError,
  )
where

import Colourista
import qualified Control.Foldl as Foldl
import qualified Data.Text as Text
import Hadolint.Formatter.Format
import Hadolint.Rule (CheckFailure (..), DLSeverity (..), RuleCode (..))
import Language.Docker.Syntax
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
import Text.Megaparsec.Stream (VisualStream)

formatError :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => ParseErrorBundle s e -> String
formatError err = stripNewlines (errorMessageLine err)

formatCheck :: Bool -> Text.Text -> CheckFailure -> Text.Text
formatCheck nocolor source CheckFailure {code, severity, line, message} =
  formatPos source line
    <> unRuleCode code
    <> " "
    <> ( if nocolor
           then severityText severity
           else colorizedSeverity severity
       )
    <> ": "
    <> message

formatPos :: Filename -> Linenumber -> Text.Text
formatPos source line = source <> ":" <> Text.pack (show line) <> " "

printResults ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e, Foldl.Foldable f) =>
  f (Result s e) ->
  Bool ->
  IO ()
printResults results color = mapM_ printResult results
  where
    printResult Result {fileName, errors, checks} = printErrors errors >> printChecks fileName checks
    printErrors errors = mapM_ (putStrLn . formatError) errors
    printChecks fileName checks = mapM_ (putStrLn . Text.unpack . formatCheck color fileName) checks

colorizedSeverity :: DLSeverity -> Text.Text
colorizedSeverity s =
  case s of
    DLErrorC -> formatWith [bold, red] $ severityText s
    DLWarningC -> formatWith [bold, yellow] $ severityText s
    DLInfoC -> formatWith [green] $ severityText s
    DLStyleC -> formatWith [cyan] $ severityText s
    _ -> severityText s
