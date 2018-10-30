{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Formatter.Codacy
    ( printResult
    , formatResult
    ) where

import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import qualified Data.Text as Text
import Hadolint.Formatter.Format (Result(..), errorPosition)
import Hadolint.Rules (Metadata(..), RuleCheck(..))
import Text.Megaparsec (Stream)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos (sourceLine, sourceName, unPos)

data Issue = Issue
    { filename :: String
    , msg :: String
    , patternId :: String
    , line :: Int
    }

instance ToJSON Issue where
    toJSON Issue {..} =
        object ["filename" .= filename, "patternId" .= patternId, "message" .= msg, "line" .= line]

errorToIssue :: (Stream s, ShowErrorComponent e) => ParseErrorBundle s e -> Issue
errorToIssue err =
    Issue
        { filename = sourceName pos
        , patternId = "DL1000"
        , msg = errorBundlePretty err
        , line = linenumber
        }
  where
    pos = errorPosition err
    linenumber = unPos (sourceLine pos)

checkToIssue :: RuleCheck -> Issue
checkToIssue RuleCheck {..} =
    Issue
        { filename = Text.unpack filename
        , patternId = Text.unpack (code metadata)
        , msg = Text.unpack (message metadata)
        , line = linenumber
        }

formatResult :: (Stream s, ShowErrorComponent e) => Result s e -> Seq Issue
formatResult (Result errors checks) = allIssues
  where
    allIssues = errorMessages <> checkMessages
    errorMessages = fmap errorToIssue errors
    checkMessages = fmap checkToIssue checks

printResult :: (Stream s, ShowErrorComponent e) => Result s e -> IO ()
printResult result = mapM_ output (formatResult result)
  where
    output value = B.putStrLn (encode value)
