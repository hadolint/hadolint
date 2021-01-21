{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hadolint.Formatter.Codeclimate
  ( printResult,
    formatResult,
  )
where

import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy as B
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import qualified Data.Text as Text
import GHC.Generics
import Hadolint.Formatter.Format (Result (..), errorPosition)
import Hadolint.Rules (Metadata (..), RuleCheck (..))
import ShellCheck.Interface
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos (sourceColumn, sourceLine, sourceName, unPos)
import Text.Megaparsec.Stream (VisualStream)

data Issue = Issue
  { checkName :: String,
    description :: String,
    location :: Location,
    impact :: String
  }

data Location
  = LocLine
      String
      Int
  | LocPos
      String
      Pos

instance ToJSON Location where
  toJSON (LocLine path l) = object ["path" .= path, "lines" .= object ["begin" .= l, "end" .= l]]
  toJSON (LocPos path pos) =
    object ["path" .= path, "positions" .= object ["begin" .= pos, "end" .= pos]]

data Pos = Pos
  { line :: Int,
    column :: Int
  }
  deriving (Generic)

instance ToJSON Pos

instance ToJSON Issue where
  toJSON Issue {..} =
    object
      [ "type" .= ("issue" :: String),
        "check_name" .= checkName,
        "description" .= description,
        "categories" .= (["Bug Risk"] :: [String]),
        "location" .= location,
        "severity" .= impact
      ]

errorToIssue :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => ParseErrorBundle s e -> Issue
errorToIssue err =
  Issue
    { checkName = "DL1000",
      description = errorBundlePretty err,
      location = LocPos (sourceName pos) Pos {..},
      impact = severityText ErrorC
    }
  where
    pos = errorPosition err
    line = unPos (sourceLine pos)
    column = unPos (sourceColumn pos)

checkToIssue :: RuleCheck -> Issue
checkToIssue RuleCheck {..} =
  Issue
    { checkName = Text.unpack (code metadata),
      description = Text.unpack (message metadata),
      location = LocLine (Text.unpack filename) linenumber,
      impact = severityText (severity metadata)
    }

severityText :: Severity -> String
severityText severity =
  case severity of
    ErrorC -> "blocker"
    WarningC -> "major"
    InfoC -> "info"
    StyleC -> "minor"

formatResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Seq Issue
formatResult (Result errors checks) = allIssues
  where
    allIssues = errorMessages <> checkMessages
    errorMessages = fmap errorToIssue errors
    checkMessages = fmap checkToIssue checks

printResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> IO ()
printResult result = B.putStrLn (encode (formatResult result))
