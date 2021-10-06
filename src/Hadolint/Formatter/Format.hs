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
    toMaybeOutputFormat,
    toResult,
  )
where

import Data.Text (unpack)
import Data.Sequence (Seq)
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
  | GitlabCodeclimateJson
  | Checkstyle
  | Codacy
  | Sarif
  deriving (Eq)

instance Read OutputFormat where
  readsPrec _ "json" = [(Json, "")]
  readsPrec _ "sonarqube" = [(SonarQube, "")]
  readsPrec _ "tty" = [(TTY, "")]
  readsPrec _ "codeclimate" = [(CodeclimateJson, "")]
  readsPrec _ "gitlab_codeclimate" = [(GitlabCodeclimateJson, "")]
  readsPrec _ "checkstyle" = [(Checkstyle, "")]
  readsPrec _ "codacy" = [(Codacy, "")]
  readsPrec _ "sarif" = [(Sarif, "")]
  readsPrec _ _ = []

instance Show OutputFormat where
  show Json = "json"
  show SonarQube = "sonarqube"
  show TTY = "tty"
  show CodeclimateJson = "codeclimate"
  show GitlabCodeclimateJson = "gitlab_codeclimate"
  show Checkstyle = "checkstyle"
  show Codacy = "codacy"
  show Sarif= "sarif"

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
withOutputFormat f (Scalar _ (SStr b)) = f $ read (unpack b)
withOutputFormat _ v = typeMismatch "output format" v


toMaybeOutputFormat :: String -> Maybe OutputFormat
toMaybeOutputFormat "json" = Just Json
toMaybeOutputFormat "sonarqube" = Just SonarQube
toMaybeOutputFormat "tty" = Just TTY
toMaybeOutputFormat "codeclimate" = Just CodeclimateJson
toMaybeOutputFormat "gitlab_codeclimate" = Just GitlabCodeclimateJson
toMaybeOutputFormat "checkstyle" = Just Checkstyle
toMaybeOutputFormat "codacy" = Just Codacy
toMaybeOutputFormat "sarif" = Just Sarif
toMaybeOutputFormat _ = Nothing

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
