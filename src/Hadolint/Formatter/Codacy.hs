module Hadolint.Formatter.Codacy
  ( printResults,
    formatResult,
  )
where

import qualified Control.Foldl as Foldl
import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Sequence (Seq)
import qualified Data.Text as Text
import Hadolint.Formatter.Format (Result (..), errorPosition)
import Hadolint.Rule (CheckFailure (..), RuleCode (..))
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos (sourceLine, sourceName, unPos)
import Text.Megaparsec.Stream (VisualStream)

data Issue = Issue
  { filename :: Text.Text,
    msg :: Text.Text,
    patternId :: Text.Text,
    line :: Int
  }

instance ToJSON Issue where
  toJSON Issue {..} =
    object ["filename" .= filename, "patternId" .= patternId, "message" .= msg, "line" .= line]

errorToIssue :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => ParseErrorBundle s e -> Issue
errorToIssue err =
  Issue
    { filename = Text.pack $ sourceName pos,
      patternId = "DL1000",
      msg = Text.pack $ errorBundlePretty err,
      line = linenumber
    }
  where
    pos = errorPosition err
    linenumber = unPos (sourceLine pos)

checkToIssue :: Text.Text -> CheckFailure -> Issue
checkToIssue filename CheckFailure {..} =
  Issue
    { filename = filename,
      patternId = unRuleCode code,
      msg = message,
      line = line
    }

formatResult :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Result s e -> Seq Issue
formatResult (Result filename errors checks) = allIssues
  where
    allIssues = errorMessages <> checkMessages
    errorMessages = fmap errorToIssue errors
    checkMessages = fmap (checkToIssue filename) checks

printResults ::
  (Foldable f, VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  f (Result s e) ->
  IO ()
printResults results = mapM_ output flattened
  where
    flattened = Foldl.fold (Foldl.premap formatResult Foldl.mconcat) results
    output value = B.putStrLn (encode value)
