{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Formatter.TTY
  ( printResult,
    formatError,
    formatChecks,
  )
where

import Colourista
import qualified Data.Text as Text
import Hadolint.Formatter.Format
import Hadolint.Rules
import Language.Docker.Syntax
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
import Text.Megaparsec.Stream (VisualStream)

formatErrors :: (VisualStream s, TraversableStream s, ShowErrorComponent e, Functor f) => f (ParseErrorBundle s e) -> f String
formatErrors = fmap formatError

formatError :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => ParseErrorBundle s e -> String
formatError err = stripNewlines (errorMessageLine err)

formatChecks :: Functor f => f RuleCheck -> Bool -> f Text.Text
formatChecks rc nocolor = fmap formatCheck rc
  where
    formatCheck (RuleCheck meta source line _) =
      formatPos source line
          <> code meta
          <> " "
          <> (if nocolor then Text.pack (severityText (severity meta))
                         else colorizedSeverity (severity meta))
          <> ": "
          <> message meta

formatPos :: Filename -> Linenumber -> Text.Text
formatPos source line = source <> ":" <> Text.pack (show line) <> " "

printResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Bool -> IO ()
printResult Result {errors, checks} color = printErrors >> printChecks
  where
    printErrors = mapM_ putStrLn (formatErrors errors)
    printChecks = mapM_ (putStrLn . Text.unpack) (formatChecks checks color)

colorizedSeverity :: DLSeverity -> Text.Text
colorizedSeverity s =
  case s of
    DLErrorC -> Text.pack $ formatWith [bold, red] $ severityText s
    DLWarningC -> Text.pack $ formatWith [bold, yellow] $ severityText s
    DLInfoC -> Text.pack $ formatWith [green] $ severityText s
    DLStyleC -> Text.pack $ formatWith [cyan] $ severityText s
    _ -> Text.pack $ severityText s
