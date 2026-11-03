module Hadolint.Formatter.Json
  ( hWrite,
    render,
  )
where

import qualified Control.Foldl as Foldl
import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Sequence (Seq)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Hadolint.Formatter.Format
  ( Result (..),
    errorPosition,
    severityText,
    errorMessage
  )
import Hadolint.Rule (CheckFailure (..), DLSeverity (..), unRuleCode)
import System.IO
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

-- | The report as text, for callers that need the value rather than a stream —
-- a JavaScript export cannot see what was written to a Handle.
render ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e, Foldable f) =>
  f (Result s e) -> Text.Text
render = TextEncoding.decodeUtf8 . B.toStrict . encode . flatten

hWrite ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e, Foldable f) =>
  Handle -> f (Result s e) -> IO ()
hWrite handle = B.hPutStr handle . encode . flatten

flatten :: Foldable f => f (Result s e) -> Seq (JsonFormat s e)
flatten = Foldl.fold (Foldl.premap formatResult Foldl.mconcat)

formatResult :: Result s e -> Seq (JsonFormat s e)
formatResult (Result fileName errors checks) = allMessages
  where
    allMessages = errorMessages <> checkMessages
    errorMessages = fmap JsonParseError errors
    checkMessages = fmap (JsonCheck fileName) checks
