-- Code Quality output formatter
--
-- GitLab CI has deprecated their CodeClimate based report format in GitLab v17.3 and plans to
-- remove it in v19.0. The CodeClimate based report format is going to be replaced by this Code
-- Quality report format, which is basically a simplified version.
--
-- See:
--   - https://docs.gitlab.com/update/deprecations/#codeclimate-based-code-quality-scanning-will-be-removed
--   - https://docs.gitlab.com/ci/testing/code_quality/#code-quality-report-format

module Hadolint.Formatter.CodeQuality
  ( hWrite )
where


import qualified Control.Foldl as Foldl
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as Char8
import Data.Sequence (Seq)
import qualified Data.Text as Text
import Hadolint.Formatter.Format (Result (..), errorPosition)
import Hadolint.Rule (CheckFailure (..), DLSeverity (..), RuleCode (..))
import System.IO
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos (sourceLine, unPos)
import Text.Megaparsec.Stream (VisualStream)

data Issue = Issue
  { checkName :: Text.Text,
    description :: Text.Text,
    location :: Location,
    impact :: Text.Text
  }

data FingerprintIssue = FingerprintIssue
  { issue :: Issue,
    fingerprint :: Char8.ByteString
  }

data Location = Location
  { path :: Text.Text,
    line :: Int
  }

instance ToJSON Location where
  toJSON Location {..} =
    object
      [ "path" .= path,
        "lines" .= object [ "begin" .= line ]
      ]

instance ToJSON Issue where
  toJSON Issue {..} =
    object
      [ "check_name" .= checkName,
        "description" .= description,
        "location" .= location,
        "severity" .= impact
      ]

instance ToJSON FingerprintIssue where
  toJSON FingerprintIssue {..} =
    object
      [ "fingerprint" .= Char8.unpack fingerprint,
        "check_name" .= checkName issue,
        "description" .= description issue,
        "location" .= location issue,
        "severity" .= impact issue
      ]

errorToIssue :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Text.Text -> ParseErrorBundle s e -> Issue
errorToIssue filename err =
  Issue
    { checkName = "DL1000",
      description = Text.pack $ errorBundlePretty err,
      location = Location filename line,
      impact = severityText DLErrorC
    }
  where
    pos = errorPosition err
    line = unPos (sourceLine pos)

checkToIssue :: Text.Text -> CheckFailure -> Issue
checkToIssue filename CheckFailure {..} =
  Issue
    { checkName = unRuleCode code,
      description = message,
      location = Location filename line,
      impact = severityText severity
    }

severityText :: DLSeverity -> Text.Text
severityText severity =
  case severity of
    DLErrorC -> "blocker"
    DLWarningC -> "major"
    DLInfoC -> "minor"
    DLStyleC -> "info"
    _ -> ""

generateFingerprint :: Issue -> Char8.ByteString
generateFingerprint = B16.encode . SHA1.hashlazy . encode

issueToFingerprintIssue :: Issue -> FingerprintIssue
issueToFingerprintIssue i =
  FingerprintIssue
    { issue = i,
      fingerprint = generateFingerprint i
    }

formatResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Maybe FilePath -> Seq FingerprintIssue
formatResult (Result filename errors checks) Nothing =
  issueToFingerprintIssue <$>
    (errorToIssue filename <$> errors) <> (checkToIssue filename <$> checks)
formatResult (Result _ errors checks) (Just filePathInReport) =
  issueToFingerprintIssue <$>
    (errorToIssue ( Text.pack filePathInReport ) <$> errors) <> (checkToIssue ( Text.pack filePathInReport ) <$> checks)

hWrite ::
  (Foldable f, VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Handle -> f (Result s e) -> Maybe FilePath -> IO ()
hWrite handle results filePathInReport = B.hPutStr handle . encode $ flattened
  where
    flattened = Foldl.fold (Foldl.premap formatResult Foldl.mconcat) results filePathInReport
