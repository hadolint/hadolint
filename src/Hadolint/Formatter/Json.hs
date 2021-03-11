module Hadolint.Formatter.Json
  ( printResults,
    formatResult,
  )
where

import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as Text
import Hadolint.Formatter.Format (Result (..), errorPosition, severityText)
import Hadolint.Rule (CheckFailure (..), DLSeverity (..), unRuleCode)
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos (sourceColumn, sourceLine, sourceName, unPos)
import Text.Megaparsec.Stream (VisualStream)

data JsonFormat s e
  = JsonCheck Text.Text CheckFailure
  | JsonParseError (ParseErrorBundle s e)

instance (VisualStream s, TraversableStream s, ShowErrorComponent e) => ToJSON (JsonFormat s e) where
  toJSON (JsonCheck filename CheckFailure {..}) =
    object
      [ "file" .= filename,
        "line" .= line,
        "column" .= (1 :: Int),
        "level" .= severityText severity,
        "code" .= unRuleCode code,
        "message" .= message
      ]
  toJSON (JsonParseError err) =
    object
      [ "file" .= sourceName pos,
        "line" .= unPos (sourceLine pos),
        "column" .= unPos (sourceColumn pos),
        "level" .= severityText DLErrorC,
        "code" .= ("DL1000" :: Text.Text),
        "message" .= errorBundlePretty err
      ]
    where
      pos = errorPosition err

printResults ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e, Foldable f) =>
  f (Result s e) ->
  IO ()
printResults = mapM_ printResult

formatResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Value
formatResult (Result fileName errors checks) = toJSON allMessages
  where
    allMessages = errorMessages <> checkMessages
    errorMessages = fmap JsonParseError errors
    checkMessages = fmap (JsonCheck fileName) checks

printResult ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Result s e ->
  IO ()
printResult result = B.putStrLn (encode (formatResult result))
