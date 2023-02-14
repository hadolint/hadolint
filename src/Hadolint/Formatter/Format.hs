module Hadolint.Formatter.Format
  ( OutputFormat (..),
    Result (..),
    Text.Megaparsec.Error.errorBundlePretty,
    errorMessage,
    errorMessageLine,
    errorPosition,
    errorPositionPretty,
    severityText,
    stripNewlines,
    readMaybeOutputFormat,
    toResult,
  )
where

import Data.Default
import Data.Sequence (Seq)
import Data.Text (Text)
import Prettyprinter (Pretty, pretty)
import Data.YAML
import Text.Megaparsec (TraversableStream (..), pstateSourcePos)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)
import Text.Megaparsec.Stream (VisualStream)
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Hadolint.Rule

data OutputFormat
  = Json
  | SonarQube
  | TTY
  | CodeclimateJson
  | GitLabCodeclimateJson
  | Gnu
  | Checkstyle
  | Codacy
  | Sarif
  deriving (Eq, Show)

instance Pretty OutputFormat where
  pretty Json = "json"
  pretty SonarQube = "sonarqube"
  pretty TTY = "tty"
  pretty CodeclimateJson = "codeclimate"
  pretty GitLabCodeclimateJson = "gitlab_codeclimate"
  pretty Gnu = "gnu"
  pretty Checkstyle = "checkstyle"
  pretty Codacy = "codacy"
  pretty Sarif = "sarif"

instance Semigroup OutputFormat where
  _ <> f = f

instance Monoid OutputFormat where
  mempty = TTY

instance FromYAML OutputFormat where
  parseYAML = withOutputFormat pure

withOutputFormat ::
  (OutputFormat -> Parser a) ->
  Node Pos ->
  Parser a
withOutputFormat f v@(Scalar _ (SStr b)) =
  case readMaybeOutputFormat b of
    Just out -> f out
    Nothing -> typeMismatch "output format" v
withOutputFormat _ v = typeMismatch "output format" v

instance Default OutputFormat where
  def = TTY

readMaybeOutputFormat :: Text -> Maybe OutputFormat
readMaybeOutputFormat "json" = Just Json
readMaybeOutputFormat "sonarqube" = Just SonarQube
readMaybeOutputFormat "tty" = Just TTY
readMaybeOutputFormat "codeclimate" = Just CodeclimateJson
readMaybeOutputFormat "gitlab_codeclimate" = Just GitLabCodeclimateJson
readMaybeOutputFormat "gnu" = Just Gnu
readMaybeOutputFormat "checkstyle" = Just Checkstyle
readMaybeOutputFormat "codacy" = Just Codacy
readMaybeOutputFormat "sarif" = Just Sarif
readMaybeOutputFormat _ = Nothing


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

errorMessage :: (VisualStream s, ShowErrorComponent e) => ParseErrorBundle s e -> String
errorMessage (ParseErrorBundle e _) =
  reverse . dropWhile (== '\n') . reverse $ parseErrorTextPretty (NE.head e)

errorPositionPretty :: TraversableStream s => ParseErrorBundle s e -> String
errorPositionPretty err = sourcePosPretty (errorPosition err)

errorPosition :: TraversableStream s => ParseErrorBundle s e -> Text.Megaparsec.Pos.SourcePos
errorPosition (ParseErrorBundle e s) =
  let (_, posState) = reachOffset (errorOffset (NE.head e)) s
   in pstateSourcePos posState
