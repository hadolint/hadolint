-- | Hadolint GNU Format Output
--
-- See https://www.gnu.org/prep/standards/html_node/Errors.html for reference.

module Hadolint.Formatter.Gnu
  ( hWrite )
where


import Data.Text (Text, pack, unpack)
import Hadolint.Formatter.Format
import Hadolint.Rule (CheckFailure (..), RuleCode (..))
import System.IO
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
import Text.Megaparsec.Stream (VisualStream)
import qualified Data.List.NonEmpty as NonEmpty

hWrite ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e, Foldable f) =>
  Handle -> f (Result s e) -> IO ()
hWrite handle = mapM_ ( printResult handle )

printResult ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Handle -> Result s e -> IO ()
printResult handle (Result filename errors checks) =
  printErrors handle errors >> printChecks handle filename checks

printErrors ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e, Foldable f) =>
  Handle -> f (ParseErrorBundle s e) -> IO ()
printErrors handle = mapM_ ( hPutStrLn handle . unpack . formatError )

printChecks :: (Foldable f) => Handle -> Text -> f CheckFailure -> IO ()
printChecks handle filename = mapM_ ( hPutStrLn handle . unpack . formatCheck filename )


formatError ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  ParseErrorBundle s e -> Text
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
