module Hadolint.Formatter.TTY
  ( hWrite,
    formatCheck,
    formatError
  )
where

import Colourista
import qualified Control.Foldl as Foldl
import qualified Data.Text as Text
import Hadolint.Formatter.Format
import Hadolint.Rule (CheckFailure (..), DLSeverity (..), RuleCode (..))
import Language.Docker.Syntax
import System.IO (Handle, hPutStrLn)
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

hWrite ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e, Foldl.Foldable f) =>
  Handle ->
  f (Result s e) ->
  Bool ->
  IO ()
hWrite handle results color = mapM_ writeResult results
  where
    writeResult Result {fileName, errors, checks} = writeErrors errors >> writeChecks fileName checks
    writeErrors = mapM_ (hPutStrLn handle . formatError)
    writeChecks fileName = mapM_ (hPutStrLn handle . Text.unpack . formatCheck color fileName)

colorizedSeverity :: DLSeverity -> Text.Text
colorizedSeverity s =
  case s of
    DLErrorC -> formatWith [bold, red] $ severityText s
    DLWarningC -> formatWith [bold, yellow] $ severityText s
    DLInfoC -> formatWith [green] $ severityText s
    DLStyleC -> formatWith [cyan] $ severityText s
    _ -> severityText s
