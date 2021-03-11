module Hadolint.Formatter.Codeclimate
  ( printResults,
    printGitlabResults,
    formatResult,
    formatGitlabResult,
  )
where

import qualified Control.Foldl as Foldl
import Crypto.Hash (Digest, SHA1 (..), hash)
import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy as B
import Data.Sequence (Seq)
import qualified Data.Text as Text
import GHC.Generics
import Hadolint.Formatter.Format (Result (..), errorPosition)
import Hadolint.Rule (CheckFailure (..), DLSeverity (..), RuleCode (..))
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos (sourceColumn, sourceLine, sourceName, unPos)
import Text.Megaparsec.Stream (VisualStream)

data Issue = Issue
  { checkName :: Text.Text,
    description :: Text.Text,
    location :: Location,
    impact :: Text.Text
  }

data FingerprintIssue = FingerprintIssue
  { issue :: Issue,
    fingerprint :: Digest SHA1
  }

data Location
  = LocLine
      Text.Text
      Int
  | LocPos
      Text.Text
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
      [ "type" .= ("issue" :: Text.Text),
        "check_name" .= checkName,
        "description" .= description,
        "categories" .= (["Bug Risk"] :: [Text.Text]),
        "location" .= location,
        "severity" .= impact
      ]

instance ToJSON FingerprintIssue where
  toJSON FingerprintIssue {..} =
    object
      [ "type" .= ("issue" :: Text.Text),
        "fingerprint" .= show fingerprint,
        "check_name" .= checkName issue,
        "description" .= description issue,
        "categories" .= (["Bug Risk"] :: [Text.Text]),
        "location" .= location issue,
        "severity" .= impact issue
      ]

errorToIssue :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => ParseErrorBundle s e -> Issue
errorToIssue err =
  Issue
    { checkName = "DL1000",
      description = Text.pack $ errorBundlePretty err,
      location = LocPos (Text.pack $ sourceName pos) Pos {..},
      impact = severityText DLErrorC
    }
  where
    pos = errorPosition err
    line = unPos (sourceLine pos)
    column = unPos (sourceColumn pos)

checkToIssue :: Text.Text -> CheckFailure -> Issue
checkToIssue fileName CheckFailure {..} =
  Issue
    { checkName = unRuleCode code,
      description = message,
      location = LocLine fileName line,
      impact = severityText severity
    }

severityText :: DLSeverity -> Text.Text
severityText severity =
  case severity of
    DLErrorC -> "blocker"
    DLWarningC -> "major"
    DLInfoC -> "info"
    DLStyleC -> "minor"
    _ -> ""

generateFingerprint :: Issue -> Digest SHA1
generateFingerprint = hash . B.toStrict . encode

issueToFingerprintIssue :: Issue -> FingerprintIssue
issueToFingerprintIssue i =
  FingerprintIssue
    { issue = i,
      fingerprint = generateFingerprint i
    }

formatResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Seq Issue
formatResult (Result filename errors checks) = (errorToIssue <$> errors) <> (checkToIssue filename <$> checks)

formatGitlabResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Seq FingerprintIssue
formatGitlabResult result = issueToFingerprintIssue <$> formatResult result

printResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> IO ()
printResult result = mapM_ output (formatResult result)
  where
    output value = do
      B.putStr (encode value)
      B.putStr (B.singleton 0x00)

printResults :: (VisualStream s, TraversableStream s, ShowErrorComponent e, Foldable f) => f (Result s e) -> IO ()
printResults = mapM_ printResult

printGitlabResults :: (Foldable f, VisualStream s, TraversableStream s, ShowErrorComponent e) => f (Result s e) -> IO ()
printGitlabResults results = B.putStr . encode $ flattened
  where
    flattened = Foldl.fold (Foldl.premap formatGitlabResult Foldl.mconcat) results
