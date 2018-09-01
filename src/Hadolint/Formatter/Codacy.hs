{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Formatter.Codacy
    ( printResult
    , formatResult
    ) where

import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List.NonEmpty as NE
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import qualified Data.Text as Text
import Hadolint.Formatter.Format (Result(..))
import Hadolint.Rules (Metadata(..), RuleCheck(..))
import Text.Megaparsec.Error
       (ParseError, ShowErrorComponent, ShowToken, errorPos,
        parseErrorTextPretty)
import Text.Megaparsec.Pos
       (sourceLine, sourceName, unPos)

data Issue = Issue
    { filename :: String
    , msg :: String
    , patternId :: String
    , line :: Int
    }

instance ToJSON Issue where
    toJSON Issue {..} =
        object
            [ "filename" .= filename
            , "patternId" .= patternId
            , "message" .= msg
            , "line" .= line
            ]

errorToIssue :: (ShowToken t, Ord t, ShowErrorComponent e) => ParseError t e -> Issue
errorToIssue err =
    Issue
    { filename = sourceName pos
    , patternId = "DL1000"
    , msg = parseErrorTextPretty err
    , line = linenumber
    }
  where
    pos = NE.head (errorPos err)
    linenumber = unPos (sourceLine pos)
    
checkToIssue :: RuleCheck -> Issue
checkToIssue RuleCheck {..} =
    Issue
    { filename = Text.unpack filename
    , patternId = Text.unpack (code metadata)
    , msg = Text.unpack (message metadata)
    , line = linenumber
    }

formatResult :: (ShowToken t, Ord t, ShowErrorComponent e) => Result t e -> Seq Issue
formatResult (Result errors checks) = allIssues
  where
    allIssues = errorMessages <> checkMessages
    errorMessages = fmap errorToIssue errors
    checkMessages = fmap checkToIssue checks

printResult :: (ShowToken t, Ord t, ShowErrorComponent e) => Result t e -> IO ()
printResult result = mapM_ output (formatResult result)
  where
    output value = B.putStrLn (encode value)
