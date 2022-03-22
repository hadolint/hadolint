-- | Hadolint GNU Format Output
--
-- See https://www.gnu.org/prep/standards/html_node/Errors.html for reference.

module Hadolint.Formatter.Gnu
  ( printResults,
  )
where


import Data.Text (Text, pack)
import Hadolint.Formatter.Format
import Hadolint.Rule (CheckFailure (..), RuleCode (..))
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
import Text.Megaparsec.Stream (VisualStream)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.IO as TextIO


printResults ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e, Foldable f) =>
  f (Result s e) ->
  IO ()
printResults = mapM_ printResult


printResult ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Result s e ->
  IO ()
printResult (Result filename errors checks) =
  printErrors errors >> printChecks filename checks


printErrors ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e, Foldable f) =>
  f (ParseErrorBundle s e) ->
  IO ()
printErrors = mapM_ (TextIO.putStrLn . formatError)


printChecks :: (Foldable f) => Text -> f CheckFailure -> IO ()
printChecks filename = mapM_ (TextIO.putStrLn . formatCheck filename)


formatError ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  ParseErrorBundle s e ->
  Text
formatError err@(ParseErrorBundle e _) =
  pack $
    "hadolint:"
      <> stripNewlines
          ( errorPositionPretty err
              <> ": "
              <> parseErrorTextPretty (NonEmpty.head e)
          )


formatCheck :: Text -> CheckFailure -> Text
formatCheck source (CheckFailure code severity message line) =
  "hadolint:"
    <> source
    <> ":"
    <> pack (show line)
    <> ": "
    <> unRuleCode code
    <> " "
    <> severityText severity
    <> ": "
    <> message
