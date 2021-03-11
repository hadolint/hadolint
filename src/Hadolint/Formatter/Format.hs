module Hadolint.Formatter.Format
  ( severityText,
    stripNewlines,
    errorMessageLine,
    errorPosition,
    errorPositionPretty,
    Text.Megaparsec.Error.errorBundlePretty,
    Result (..),
    toResult,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Hadolint.Rule
import Text.Megaparsec (TraversableStream (..), pstateSourcePos)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)
import Text.Megaparsec.Stream (VisualStream)

data Result s e = Result
  { fileName :: Text.Text,
    errors :: Seq (ParseErrorBundle s e),
    checks :: Hadolint.Rule.Failures
  }

toResult :: Text.Text -> Either (ParseErrorBundle s e) Hadolint.Rule.Failures -> Result s e
toResult file res =
  case res of
    Left err -> Result file (Seq.singleton err) mempty
    Right c -> Result file mempty (Seq.unstableSort c)

severityText :: Hadolint.Rule.DLSeverity -> Text.Text
severityText s =
  case s of
    Hadolint.Rule.DLErrorC -> "error"
    Hadolint.Rule.DLWarningC -> "warning"
    Hadolint.Rule.DLInfoC -> "info"
    Hadolint.Rule.DLStyleC -> "style"
    _ -> ""

stripNewlines :: String -> String
stripNewlines =
  map
    ( \c ->
        if c == '\n'
          then ' '
          else c
    )

errorMessageLine :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => ParseErrorBundle s e -> String
errorMessageLine err@(ParseErrorBundle e _) =
  errorPositionPretty err ++ " " ++ parseErrorTextPretty (NE.head e)

errorPositionPretty :: TraversableStream s => ParseErrorBundle s e -> String
errorPositionPretty err = sourcePosPretty (errorPosition err)

errorPosition :: TraversableStream s => ParseErrorBundle s e -> Text.Megaparsec.Pos.SourcePos
errorPosition (ParseErrorBundle e s) =
  let (_, posState) = reachOffset (errorOffset (NE.head e)) s
   in pstateSourcePos posState
