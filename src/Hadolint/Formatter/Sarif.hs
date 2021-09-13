module Hadolint.Formatter.Sarif
  ( printResults,
    formatResult,
  )
where

import qualified Control.Foldl as Foldl
import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Sequence as Seq
import qualified Data.Text as Text
import Hadolint.Formatter.Format
  ( Result (..),
    errorMessage,
    errorPosition,
  )
import Hadolint.Meta
  ( getShortVersion,
  )
import Hadolint.Rule
  ( CheckFailure (..),
    DLSeverity (..),
    unRuleCode,
  )
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
  ( sourceColumn,
    sourceLine,
    sourceName,
    unPos,
  )
import Text.Megaparsec.Stream (VisualStream)

data SarifFormat s e
  = SarifCheck Text.Text CheckFailure
  | SarifError (ParseErrorBundle s e)

instance
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  ToJSON (SarifFormat s e)
  where
  toJSON (SarifCheck filename CheckFailure {..}) =
    object
      [ "ruleId" .= unRuleCode code,
        "level" .= toSeverity severity,
        "message"
          .= object
            [ "text" .= message
            ],
        "locations"
          .= [ object
                 [ "physicalLocation"
                     .= object
                       [ "artifactLocation"
                           .= object
                             [ "uri" .= filename
                             ],
                         "region"
                           .= object
                             [ "startLine" .= line,
                               "endLine" .= line,
                               "startColumn" .= (1 :: Int),
                               "endColumn" .= (1 :: Int),
                               "sourceLanguage" .= Text.pack language
                             ]
                       ]
                 ]
             ]
      ]
    where
      language = if "DL" `Text.isPrefixOf` unRuleCode code
                    then "dockerfile"
                    else "sh"
  toJSON (SarifError err) =
    object
      [ "ruleId" .= Text.pack "DL1000",
        "level" .= Text.pack "error",
        "message"
          .= object
            [ "text" .= errorMessage err
            ],
        "locations"
          .= [ object
                 [ "physicalLocation"
                     .= object
                       [ "artifactLocation"
                           .= object
                             [ "uri" .= Text.pack (sourceName pos)
                             ],
                         "region"
                           .= object
                             [ "startLine" .= linenumber,
                               "endLine" .= linenumber,
                               "startColumn" .= column,
                               "endColumn" .= column,
                               "sourceLanguage" .= Text.pack "dockerfile"
                             ]
                       ]
                 ]
             ]
      ]
    where
      pos = errorPosition err
      linenumber = unPos $ sourceLine pos
      column = unPos $ sourceColumn pos

formatResult :: Result s e -> Seq (SarifFormat s e)
formatResult (Result filename errors checks) = allMessages
  where
    allMessages = errorMessages <> checkMessages
    checkMessages = fmap (SarifCheck filename) checks
    errorMessages = fmap SarifError errors

printResults ::
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e,
    Foldable f
  ) =>
  f (Result s e) ->
  IO ()
printResults results =
  B.putStr . encode $
    object
      [ ("version", "2.1.0"),
        "$schema"
          .= Text.pack "http://json.schemastore.org/sarif-2.1.0",
        "runs"
          .= [ object
                 [ "tool"
                     .= object
                       [ "driver"
                           .= object
                             [ ("name", "Hadolint"),
                               ("fullName", "Haskell Dockerfile Linter"),
                               ("downloadUri",
                                  "https://github.com/hadolint/hadolint"),
                               "version"
                                 .= Text.pack Hadolint.Meta.getShortVersion,
                               "shortDescription"
                                 .= object
                                   [ ("text",
  "Dockerfile linter, validate inline bash, written in Haskell")
                                   ]
                             ]
                       ],
                   "results" .= flattened,
                   "defaultSourceLanguage" .= Text.pack "dockerfile"
                 ]
             ]
      ]
  where
    flattened = Foldl.fold (Foldl.premap formatResult Foldl.mconcat) results

-- | SARIF only specifies three severities "error", "warning" and "note"
-- We pack our "info" and "style" severities together into the "note" severity
-- here.
toSeverity :: DLSeverity -> Text.Text
toSeverity DLErrorC = "error"
toSeverity DLWarningC = "warning"
toSeverity _ = "note"
