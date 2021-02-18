{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hadolint.Formatter.Json
  ( printResult,
    formatResult,
  )
where

import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as B
import Hadolint.Formatter.Format (Result (..), errorPosition, severityText)
import Hadolint.Rules (Metadata (..), RuleCheck (..), DLSeverity (..))
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos (sourceColumn, sourceLine, sourceName, unPos)
import Text.Megaparsec.Stream (VisualStream)

data JsonFormat s e
  = JsonCheck RuleCheck
  | JsonParseError (ParseErrorBundle s e)

instance (VisualStream s, TraversableStream s, ShowErrorComponent e) => ToJSON (JsonFormat s e) where
  toJSON (JsonCheck RuleCheck {..}) =
    object
      [ "file" .= filename,
        "line" .= linenumber,
        "column" .= (1 :: Int),
        "level" .= severityText (severity metadata),
        "code" .= code metadata,
        "message" .= message metadata
      ]
  toJSON (JsonParseError err) =
    object
      [ "file" .= sourceName pos,
        "line" .= unPos (sourceLine pos),
        "column" .= unPos (sourceColumn pos),
        "level" .= severityText DLErrorC,
        "code" .= ("DL1000" :: String),
        "message" .= errorBundlePretty err
      ]
    where
      pos = errorPosition err

formatResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Value
formatResult (Result errors checks) = toJSON allMessages
  where
    allMessages = errorMessages <> checkMessages
    errorMessages = fmap JsonParseError errors
    checkMessages = fmap JsonCheck checks

printResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> IO ()
printResult result = B.putStrLn (encode (formatResult result))
