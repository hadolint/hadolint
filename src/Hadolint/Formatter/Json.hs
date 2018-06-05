{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Formatter.Json
    ( printResult
    , formatResult
    ) where

import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List.NonEmpty as NE
import Data.Monoid ((<>))
import Hadolint.Formatter.Format (Result(..), severityText)
import Hadolint.Rules (Metadata(..), RuleCheck(..))
import ShellCheck.Interface
import Text.Megaparsec.Error
       (ParseError, ShowErrorComponent, ShowToken, errorPos,
        parseErrorTextPretty)
import Text.Megaparsec.Pos
       (sourceColumn, sourceLine, sourceName, unPos)

data JsonFormat t e
    = JsonCheck RuleCheck
    | JsonParseError (ParseError t e)

instance (ShowToken t, Ord t, ShowErrorComponent e) => ToJSON (JsonFormat t e) where
    toJSON (JsonCheck RuleCheck {..}) =
        object
            [ "file" .= filename
            , "line" .= linenumber
            , "column" .= (1 :: Int)
            , "level" .= severityText (severity metadata)
            , "code" .= code metadata
            , "message" .= message metadata
            ]
    toJSON (JsonParseError err) =
        object
            [ "file" .= sourceName pos
            , "line" .= unPos (sourceLine pos)
            , "column" .= unPos (sourceColumn pos)
            , "level" .= severityText ErrorC
            , "code" .= ("DL1000" :: String)
            , "message" .= parseErrorTextPretty err
            ]
      where
        pos = NE.head (errorPos err)

formatResult :: (ShowToken t, Ord t, ShowErrorComponent e) => Result t e -> Value
formatResult (Result errors checks) = toJSON allMessages
  where
    allMessages = errorMessages <> checkMessages
    errorMessages = fmap JsonParseError errors
    checkMessages = fmap JsonCheck checks

printResult :: (ShowToken t, Ord t, ShowErrorComponent e) => Result t e -> IO ()
printResult result = B.putStrLn (encode (formatResult result))
