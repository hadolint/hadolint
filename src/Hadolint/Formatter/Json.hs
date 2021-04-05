module Hadolint.Formatter.Json
  ( printResults,
    formatResult,
  )
where

import qualified Control.Foldl as Foldl
import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Sequence (Seq)
import qualified Data.Text as Text
import Hadolint.Formatter.Format
  ( Result (..),
    errorPosition,
    severityText,
    errorMessage
  )
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
        "message" .= errorMessage err
      ]
    where
      pos = errorPosition err

printResults ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e, Foldable f) =>
  f (Result s e) ->
  IO ()
printResults results = B.putStr . encode $ flattened
  where
    flattened = Foldl.fold (Foldl.premap formatResult Foldl.mconcat) results

formatResult :: Result s e -> Seq (JsonFormat s e)
formatResult (Result fileName errors checks) = allMessages
  where
    allMessages = errorMessages <> checkMessages
    errorMessages = fmap JsonParseError errors
    checkMessages = fmap (JsonCheck fileName) checks
