{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hadolint.Formatter.Codeclimate
  ( printResult,
    printGitlabResult,
    formatResult,
  )
where

import Crypto.Hash (Digest, SHA1 (..), hash)
import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy as B
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

data FingerprintIssue = FingerprintIssue
  { issue :: Issue,
    fingerprint :: Digest SHA1
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

instance ToJSON FingerprintIssue where
  toJSON FingerprintIssue {..} =
    object
      [ "type" .= ("issue" :: String),
        "fingerprint" .= show fingerprint,
        "check_name" .= checkName issue,
        "description" .= description issue,
        "categories" .= (["Bug Risk"] :: [String]),
        "location" .= location issue,
        "severity" .= impact issue
      ]

errorToIssue :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => ParseErrorBundle s e -> Issue
errorToIssue err =
  Issue
    { checkName = "DL1000",
      description = errorBundlePretty err,
      location = LocPos (sourceName pos) Pos {..},
      impact = severityText (Just ErrorC)
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

severityText :: Maybe Severity -> String
severityText severity =
  case severity of
    Just ErrorC -> "blocker"
    Just WarningC -> "major"
    Just InfoC -> "info"
    Just StyleC -> "minor"
    Nothing -> "nothing"

generateFingerprint :: Issue -> Digest SHA1
generateFingerprint = hash . B.toStrict . encode

issueToFingerprintIssue :: Issue -> FingerprintIssue
issueToFingerprintIssue i =
  FingerprintIssue
    { issue = i,
      fingerprint = generateFingerprint i
    }

formatResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Seq Issue
formatResult (Result errors checks) = (errorToIssue <$> errors) <> (checkToIssue <$> checks)

formatGitlabResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Seq FingerprintIssue
formatGitlabResult result = issueToFingerprintIssue <$> formatResult result

printResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> IO ()
printResult result = mapM_ output (formatResult result)
  where
    output value = do
      B.putStr (encode value)
      B.putStr (B.singleton 0x00)

printGitlabResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> IO ()
printGitlabResult = B.putStr . encode . formatGitlabResult
