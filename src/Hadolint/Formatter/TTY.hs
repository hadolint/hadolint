{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Formatter.TTY
  ( printResult,
    formatError,
    formatChecks,
  )
where

import Data.Semigroup ((<>))
import qualified Data.Text as Text
import Hadolint.Formatter.Format
import Hadolint.Rules
import Language.Docker.Syntax
import ShellCheck.Interface (Severity (..))
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
import Text.Megaparsec.Stream (VisualStream)

formatErrors :: (VisualStream s, TraversableStream s, ShowErrorComponent e, Functor f) => f (ParseErrorBundle s e) -> f String
formatErrors = fmap formatError

formatError :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => ParseErrorBundle s e -> String
formatError err = stripNewlines (errorMessageLine err)

formatChecks :: Functor f => f RuleCheck -> Bool -> f Text.Text
formatChecks rc color = fmap formatCheck rc
  where
    formatCheck (RuleCheck meta source line _) =
      formatPos source line
          <> code meta
          <> " "
          <> (if color then Text.pack (colorFromSeverity (severity meta)) else "")
          <> Text.pack (severityText (severity meta))
          <> (if color then Text.pack (ansi 0) else "")
          <> ": "
          <> message meta

formatPos :: Filename -> Linenumber -> Text.Text
formatPos source line = source <> ":" <> Text.pack (show line) <> " "

printResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Bool -> IO ()
printResult Result {errors, checks} color = printErrors >> printChecks
  where
    printErrors = mapM_ putStrLn (formatErrors errors)
    printChecks = mapM_ (putStrLn . Text.unpack) (formatChecks checks color)

colorFromSeverity :: Severity -> String
colorFromSeverity s =
  case s of
    ErrorC -> ansi 1 ++ ansi 31    -- bold red
    WarningC -> ansi 1 ++ ansi 33  -- bold yellow
    InfoC -> ansi 32               -- green
    StyleC  -> ansi 36             -- cyan

ansi :: Int -> String
ansi n = "\x1B[" ++ show n ++ "m"
