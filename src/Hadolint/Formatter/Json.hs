{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Formatter.Json
    ( formatChecks
    , formatError
    , printResults
    ) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Hadolint.Formatter.Format (formatErrorReason, severityText)
import Hadolint.Rules (Metadata(..), RuleCheck(..))
import ShellCheck.Interface
import Text.Parsec (ParseError)
import Text.Parsec.Error (ParseError, errorPos)
import Text.Parsec.Pos

newtype JsonFormat =
    JsonFormat RuleCheck

newtype JsonParseError =
    JsonParseError ParseError

instance ToJSON JsonFormat where
    toJSON (JsonFormat RuleCheck {..}) =
        object
            [ "file" .= filename
            , "line" .= linenumber
            , "column" .= (1 :: Int)
            , "level" .= severityText (severity metadata)
            , "code" .= code metadata
            , "message" .= message metadata
            ]

instance ToJSON JsonParseError where
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

formatError :: ParseError -> ByteString
formatError err = encode (JsonParseError err)

formatChecks :: [RuleCheck] -> ByteString
formatChecks checks = encode (map JsonFormat checks)

printResults :: Either ParseError [RuleCheck] -> IO ()
printResults = either printError printChecks
  where
    printError = B.putStrLn . formatError
    printChecks = B.putStrLn . formatChecks
