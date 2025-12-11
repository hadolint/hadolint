module Hadolint.Formatter.SonarQube
  ( formatResult,
    printResults
  )
  where

import qualified Control.Foldl as Foldl
import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Sequence (Seq)
import qualified Data.Text as Text
import Hadolint.Formatter.Format
  ( Result (..),
    errorPosition,
    errorMessage
  )
import Hadolint.Rule
  ( CheckFailure (..),
    DLSeverity (..),
    unRuleCode
  )
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
  ( sourceColumn,
    sourceLine,
    unPos
  )
import Text.Megaparsec.Stream (VisualStream)


data SonarQubeFormat s e
  = SonarQubeCheck Text.Text CheckFailure
  | SonarQubeError Text.Text (ParseErrorBundle s e)

instance (VisualStream s,
  TraversableStream s,
  ShowErrorComponent e) => ToJSON (SonarQubeFormat s e) where
  toJSON (SonarQubeCheck filename CheckFailure {..}) =
    object
      [ "engineId" .= Text.pack "Hadolint",
        "ruleId" .= unRuleCode code,
        "severity" .= toSeverity severity,
        "type" .= toType severity,
        "primaryLocation" .= object
          [ "message" .= message,
            "filePath" .= filename,
            "textRange" .= object
              [ "startLine" .= line,
                "endLine" .= line,
                "startColumn" .= (0 :: Int),
                "endColumn" .= (1 :: Int)
              ]
          ]
      ]
  toJSON (SonarQubeError filename err) =
    object
      [ "engineId" .= Text.pack "Hadolint",
        "ruleId" .= Text.pack "DL1000",
        "severity" .= Text.pack "BLOCKER",
        "type" .= Text.pack "BUG",
        "primaryLocation" .= object
          [ "message" .= errorMessage err,
            "filePath" .= filename,
            "textRange" .= object
              [ "startLine" .= linenumber,
                "endLine" .= linenumber,
                "startColumn" .= column,
                "endColumn" .= column
              ]
          ]
      ]
    where
      pos = errorPosition err
      linenumber = unPos $ sourceLine pos
      column = unPos $ sourceColumn pos


formatResult :: Maybe FilePath -> Result s e -> Seq (SonarQubeFormat s e)
formatResult filePathInReport (Result filename errors checks) = allMessages
  where
    allMessages = errorMessages <> checkMessages
    errorMessages = fmap (SonarQubeError filepath) errors
    checkMessages = fmap (SonarQubeCheck filepath) checks
    filepath = if null filePathInReport then filename else getFilePath filePathInReport

printResults :: (VisualStream s,
  TraversableStream s,
  ShowErrorComponent e,
  Foldable f) => f (Result s e) -> Maybe FilePath -> IO ()
printResults results filePathInReport =
  B.putStr . encode $ object [ "issues" .= flattened ]
  where
    flattened = Foldl.fold (Foldl.premap (formatResult filePathInReport) Foldl.mconcat) results

toType :: DLSeverity -> Text.Text
toType DLErrorC = "BUG"
toType _ = "CODE_SMELL"

toSeverity :: DLSeverity -> Text.Text
toSeverity DLErrorC = "CRITICAL"
toSeverity DLWarningC = "MAJOR"
toSeverity DLInfoC = "MINOR"
toSeverity _ = "INFO"

getFilePath :: Maybe FilePath -> Text.Text
getFilePath Nothing = ""
getFilePath (Just filePath) = toText [ filePath ]

toText :: [FilePath] -> Text.Text
toText = foldMap Text.pack
