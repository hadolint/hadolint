module Hadolint.Formatter.Checkstyle ( printResults )
where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Hadolint.Formatter.Format
  ( Result (..),
    errorBundlePretty,
    errorPosition,
    severityText,
  )
import Hadolint.Rule (CheckFailure (..), DLSeverity (..), RuleCode (..))
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Error
  ( ParseErrorBundle,
    ShowErrorComponent,
  )
import Text.Megaparsec.Pos (sourceColumn, sourceLine, unPos)
import Text.Megaparsec.Stream (VisualStream)
import qualified Text.XML as XML

errorToNode ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  ParseErrorBundle s e -> XML.Node
errorToNode err =
  XML.NodeElement XML.Element
    { elementName = "error",
      elementAttributes =
        Map.fromList
          [ ("line", Text.pack $ show $ unPos $ sourceLine $ errorPosition err),
            ("column", Text.pack $ show $ unPos $ sourceColumn $ errorPosition err),
            ("severity", severityText DLErrorC),
            ("message", Text.pack $ errorBundlePretty err)
          ],
      elementNodes = []
    }

checkToNode :: CheckFailure -> XML.Node
checkToNode CheckFailure {..} =
  XML.NodeElement XML.Element
    { elementName = "error",
      elementAttributes =
        Map.fromList
          [ ("line", Text.pack $ show line),
            ("column", "1"),
            ("severity", severityText severity),
            ("message", message),
            ("source", unRuleCode code)
          ],
      elementNodes = []
    }

renderNodes ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Result s e -> [XML.Node]
renderNodes (Result _ errors checks) =
  if isEmpty then [] else toList ( errorNodes <> checkNodes )
  where
    errorNodes = fmap errorToNode errors
    checkNodes = fmap checkToNode checks
    isEmpty = null checks && null errors

toFile ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Result s e -> Maybe FilePath -> XML.Node
toFile results filePathInReport =
  XML.NodeElement XML.Element
    { elementName = "file",
      elementAttributes = Map.fromList [("name", filepath)],
      elementNodes = renderNodes results
    }
  where
    filepath = if null filePathInReport then filename results else getFilePath filePathInReport
    filename (Result {fileName=fn}) = fn

renderResults ::
  (Foldable f, VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  f (Result s e) -> Maybe FilePath -> XML.Element
renderResults results filePathInReport = XML.Element
  { elementName = "checkstyle",
    elementAttributes = Map.fromList [("version", "4.3")],
    elementNodes = Maybe.catMaybes $ map maybeFile ( toList results )
  }
  where
    maybeFile r = if isEmpty r then Nothing else Just $ toFile r filePathInReport
    isEmpty (Result {errors=e, checks=c}) = null e && null c

printResults ::
  (Foldable f, VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  f (Result s e) -> Maybe FilePath -> IO ()
printResults results filePathInReport =
  B.putStr $ XML.renderLBS settings document
  where
    settings = XML.def -- use default render settings
    document =
      XML.Document
        { documentPrologue = XML.Prologue [] Nothing [],
          documentRoot = renderResults results filePathInReport,
          documentEpilogue = []
        }

getFilePath :: Maybe FilePath -> Text.Text
getFilePath Nothing = ""
getFilePath (Just filePath) = toText [filePath]

toText :: [FilePath] -> Text.Text
toText = foldMap Text.pack
