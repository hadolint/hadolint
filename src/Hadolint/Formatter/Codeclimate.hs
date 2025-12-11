module Hadolint.Formatter.Codeclimate
  ( printResults,
    printGitLabResults,
    formatResult,
    formatGitLabResult,
  )
where

import qualified Control.Foldl as Foldl
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as Char8
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
    fingerprint :: Char8.ByteString
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
        "fingerprint" .= Char8.unpack fingerprint,
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

checkToIssue :: Text.Text -> Maybe FilePath -> CheckFailure -> Issue
checkToIssue fileName filePathInReport CheckFailure {..} =
  Issue
    { checkName = unRuleCode code,
      description = message,
      location = LocLine reportFileName line,
      impact = severityText severity
    }
  where
    reportFileName = if null filePathInReport then fileName else getFilePath filePathInReport

severityText :: DLSeverity -> Text.Text
severityText severity =
  case severity of
    DLErrorC -> "blocker"
    DLWarningC -> "major"
    DLInfoC -> "info"
    DLStyleC -> "minor"
    _ -> ""

generateFingerprint :: Issue -> Char8.ByteString
generateFingerprint = B16.encode . SHA1.hashlazy . encode

issueToFingerprintIssue :: Issue -> FingerprintIssue
issueToFingerprintIssue i =
  FingerprintIssue
    { issue = i,
      fingerprint = generateFingerprint i
    }

formatResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Maybe FilePath -> Seq Issue
formatResult (Result filename errors checks) filePathInReport = (errorToIssue <$> errors) <> (checkToIssue filename filePathInReport <$> checks)

formatGitLabResult ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Result s e -> Maybe FilePath ->
  Seq FingerprintIssue
formatGitLabResult result filePathInReport = issueToFingerprintIssue <$> (formatResult result filePathInReport)

printResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Maybe FilePath -> IO ()
printResult result filePathInReport = mapM_ output (formatResult result filePathInReport)
  where
    output value = do
      B.putStr (encode value)
      B.putStr (B.singleton 0x00)

printResults :: (VisualStream s, TraversableStream s, ShowErrorComponent e, Foldable f) => f (Result s e) -> Maybe FilePath -> IO ()
printResults results filePathInReport = flattened
  where
    flattened = Foldl.fold (Foldl.premap printResult Foldl.mconcat) results filePathInReport

printGitLabResults ::
  (Foldable f, VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  f (Result s e) -> Maybe FilePath ->
  IO ()
printGitLabResults results filePathInReport = B.putStr . encode $ flattened
  where
    flattened = Foldl.fold (Foldl.premap formatGitLabResult Foldl.mconcat) results filePathInReport

getFilePath :: Maybe FilePath -> Text.Text
getFilePath Nothing = ""
getFilePath (Just filePath) = toText [filePath]

toText :: [FilePath] -> Text.Text
toText = foldMap Text.pack
