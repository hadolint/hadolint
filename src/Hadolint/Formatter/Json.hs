{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Formatter.Json
    ( printResult
    , formatResult
    ) where

import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Monoid ((<>))
import Hadolint.Formatter.Format
       (Result(..), formatErrorReason, severityText)
import Hadolint.Rules (Metadata(..), RuleCheck(..))
import ShellCheck.Interface
import Text.Parsec (ParseError)
import Text.Parsec.Error (ParseError, errorPos)
import Text.Parsec.Pos

data JsonFormat
    = JsonCheck RuleCheck
    | JsonParseError ParseError

instance ToJSON JsonFormat where
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
            , "line" .= sourceLine pos
            , "column" .= sourceColumn pos
            , "level" .= severityText ErrorC
            , "code" .= ("DL1000" :: String)
            , "message" .= formatErrorReason err
            ]
      where
        pos = errorPos err

formatResult :: Result -> Value
formatResult (Result errors checks) = toJSON allMessages
  where
    allMessages = errorMessages <> checkMessages
    errorMessages = fmap JsonParseError errors
    checkMessages = fmap JsonCheck checks

printResult :: Result -> IO ()
printResult result = B.putStrLn (encode (formatResult result))
