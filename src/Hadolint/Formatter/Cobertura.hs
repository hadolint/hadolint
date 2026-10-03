module Hadolint.Formatter.Cobertura ( hWrite ) where


import qualified Data.ByteString.Lazy.Char8 as B
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as Time
import Hadolint.Formatter.Format
  ( Result (..),
    errorPosition,
  )
import Hadolint.Meta (getShortVersion)
import Hadolint.Rule (CheckFailure (..))
import System.IO (Handle)
import Text.Megaparsec (TraversableStream)
import Text.Megaparsec.Pos (sourceLine, unPos)
import qualified Text.XML as XML


hWrite ::
  (Foldable f, TraversableStream s) =>
  Handle -> f (Result s e) -> Maybe FilePath -> IO ()
hWrite handle results filePathInReport = do
  timestamp <- round <$> Time.getPOSIXTime
  B.hPutStr handle $ XML.renderLBS XML.def $ document timestamp
  where
    resultList = toList results
    validLines = sum $ fmap (Set.size . resultLines) resultList

    document timestamp =
      XML.Document
        { documentPrologue = XML.Prologue [] Nothing [],
          documentRoot = coverageNode timestamp validLines resultList filePathInReport,
          documentEpilogue = []
        }


coverageNode ::
  TraversableStream s =>
  Integer -> Int -> [Result s e] -> Maybe FilePath -> XML.Element
coverageNode timestamp validLines results filePathInReport =
  XML.Element
    { elementName = "coverage",
      elementAttributes =
        Map.fromList
          [ ("line-rate", lineRate validLines),
            ("branch-rate", "0.0"),
            ("lines-covered", "0"),
            ("lines-valid", showText validLines),
            ("branches-covered", "0"),
            ("branches-valid", "0"),
            ("complexity", "0.0"),
            ("version", Text.pack getShortVersion),
            ("timestamp", showText timestamp)
          ],
      elementNodes =
        [ XML.NodeElement sourcesNode,
          XML.NodeElement $ packagesNode validLines results filePathInReport
        ]
    }


sourcesNode :: XML.Element
sourcesNode =
  XML.Element
    { elementName = "sources",
      elementAttributes = mempty,
      elementNodes = [XML.NodeElement sourceNode]
    }
  where
    sourceNode =
      XML.Element
        { elementName = "source",
          elementAttributes = mempty,
          elementNodes = [XML.NodeContent "."]
        }


packagesNode ::
  TraversableStream s =>
  Int -> [Result s e] -> Maybe FilePath -> XML.Element
packagesNode validLines results filePathInReport =
  XML.Element
    { elementName = "packages",
      elementAttributes = mempty,
      elementNodes = [XML.NodeElement packageNode]
    }
  where
    packageNode =
      XML.Element
        { elementName = "package",
          elementAttributes =
            Map.fromList
              [ ("name", "hadolint"),
                ("line-rate", lineRate validLines),
                ("branch-rate", "0.0"),
                ("complexity", "0.0")
              ],
          elementNodes =
            [ XML.NodeElement
                XML.Element
                  { elementName = "classes",
                    elementAttributes = mempty,
                    elementNodes = fmap (XML.NodeElement . classNode filePathInReport) results
                  }
            ]
        }


classNode ::
  TraversableStream s =>
  Maybe FilePath -> Result s e -> XML.Element
classNode filePathInReport result =
  XML.Element
    { elementName = "class",
      elementAttributes =
        Map.fromList
          [ ("name", filepath),
            ("filename", filepath),
            ("line-rate", lineRate $ Set.size failureLines),
            ("branch-rate", "0.0"),
            ("complexity", "0.0")
          ],
      elementNodes =
        [ XML.NodeElement
            XML.Element
              { elementName = "methods",
                elementAttributes = mempty,
                elementNodes = []
              },
          XML.NodeElement
            XML.Element
              { elementName = "lines",
                elementAttributes = mempty,
                elementNodes = lineNode <$> Set.toAscList failureLines
              }
        ]
    }
  where
    filepath = maybe (fileName result) Text.pack filePathInReport
    failureLines = resultLines result


lineNode :: Int -> XML.Node
lineNode number =
  XML.NodeElement
    XML.Element
      { elementName = "line",
        elementAttributes =
          Map.fromList
            [ ("number", showText number),
              ("hits", "0")
            ],
        elementNodes = []
      }


resultLines :: TraversableStream s => Result s e -> Set.Set Int
resultLines Result {errors, checks} =
  Set.fromList $ fmap errorLine (toList errors) <> fmap line (toList checks)
  where
    errorLine = unPos . sourceLine . errorPosition


lineRate :: Int -> Text.Text
lineRate 0 = "1.0"
lineRate _ = "0.0"


showText :: Show a => a -> Text.Text
showText = Text.pack . show
