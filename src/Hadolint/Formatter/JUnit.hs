module Hadolint.Formatter.JUnit ( printResults ) where


import qualified Data.ByteString.Lazy.Char8 as B
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time as Time
import Hadolint.Formatter.Format
  ( Result (..),
    errorBundlePretty,
    errorPosition,
    severityText,
  )
import Hadolint.Rule (CheckFailure (..), RuleCode (..))
import Hadolint.Meta (getShortVersion)
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
  ( ParseErrorBundle,
    ShowErrorComponent,
  )
import Text.Megaparsec.Pos (sourceColumn, sourceLine, unPos)
import Text.Megaparsec.Stream (VisualStream)
import qualified Text.XML as XML


providerName :: Text.Text
providerName = "Hadolint " <> Text.pack getShortVersion

providerID :: Text.Text
providerID = "hadolint"


printResults ::
  (Foldable f, VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  f (Result s e) -> Maybe FilePath -> IO ()
printResults results maybeFilepath = do
  time <- Time.getCurrentTime
  B.putStr $ XML.renderLBS settings $ document time

  where
    settings = XML.def

    document :: Time.UTCTime -> XML.Document
    document t =
      XML.Document
        { documentPrologue = XML.Prologue [] Nothing [],
          documentRoot = root t,
          documentEpilogue = []

        }

    root :: Time.UTCTime -> XML.Element
    root t =
      XML.Element
        { elementName = "testsuites",
          elementAttributes =
            Map.fromList
              [ ("id", runID t),
                ("name", runName t),
                ("time", "0.001")
              ],
          elementNodes = fmap ( `renderResult` maybeFilepath ) ( toList results )
        }

    runID :: Time.UTCTime -> Text.Text
    runID t =
      Text.pack $ Time.formatTime Time.defaultTimeLocale "%Y%m%d_%H%M%S" t

    runName :: Time.UTCTime -> Text.Text
    runName t =
      Text.pack $ Time.formatTime Time.defaultTimeLocale "Hadolint run at %Y-%m-%d %H:%M:%S" t

renderResult ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Result s e -> Maybe FilePath -> XML.Node
renderResult (Result filename errors checks) maybeFilepath =
  XML.NodeElement XML.Element
    { elementName = "testsuite",
      elementAttributes =
        Map.fromList
          [ ("id", providerID),
            ("name", providerName),
            ("time", "0.001"),
            ("failures", Text.pack $ show $ length findings),
            ("errors", Text.pack $ show parseErrors)
          ],
      elementNodes = findings
    }
  where
    findings = toList ( errorNodes <> checkNodes )
    parseErrors = length errorNodes
    errorNodes = fmap ( `errorToNode` file ) errors
    checkNodes = fmap ( `checkToNode` file ) checks
    file = if null maybeFilepath then filename else Text.pack $ fromMaybe "" maybeFilepath

errorToNode ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  ParseErrorBundle s e -> Text.Text -> XML.Node
errorToNode err filename =
  XML.NodeElement XML.Element
    { elementName = "testcase",
      elementAttributes =
        Map.fromList
          [ ("id", providerID <> ".error"),
            ("name", "DL1000"),
            ("time", "0.001")
          ],
      elementNodes =
        [ XML.NodeElement XML.Element
            { elementName = "failure",
              elementAttributes =
                Map.fromList
                  [ ("type", "error"),
                    ("message", "parse error at line " <> line <> ", column " <> column)
                  ],
              elementNodes =
                [ XML.NodeContent $
                    Text.unlines
                      [ "Parse Error in " <> filename,
                        "Line: " <> line,
                        "Column: " <> column,
                        "Parse Error: " <> Text.pack ( errorBundlePretty err )
                      ]
                ]
            }
        ]
    }
  where
    line = Text.pack ( show $ unPos $ sourceLine $ errorPosition err )
    column = Text.pack ( show $ unPos $ sourceColumn $ errorPosition err )

checkToNode :: CheckFailure -> Text.Text -> XML.Node
checkToNode CheckFailure {..} filename =
  XML.NodeElement XML.Element
    { elementName = "testcase",
      elementAttributes =
        Map.fromList
          [ ("id", providerID <> ".rule." <> unRuleCode code),
            ("time", "0.001")
          ],
      elementNodes =
        [ XML.NodeElement XML.Element
            { elementName = "failure",
              elementAttributes =
                Map.fromList
                  [ ("type", severityText severity),
                    ("message", message),
                    ("id", unRuleCode code)
                  ],
              elementNodes =
                [ XML.NodeContent $
                    Text.unlines
                      [ "File: " <> filename,
                        "Line: " <> Text.pack ( show line ),
                        "Category: Hadolint - Dockerfile Static Analysis",
                        severityText severity <> ": " <> message
                      ]
                ]
            }
        ]
    }

