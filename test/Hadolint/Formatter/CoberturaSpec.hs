module Hadolint.Formatter.CoberturaSpec ( spec ) where

import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Hadolint
  ( CheckFailure (..),
    DLSeverity (..),
    OutputFormat (..),
    getShortVersion,
    write,
  )
import Hadolint.Formatter.Format (Result (..))
import System.IO.Silently (capture)
import Test.Hspec
import qualified Data.Text as Text
import qualified Text.XML as XML


spec :: SpecWith ()
spec = do
  let ?noColor = True

  describe "Formatter: Cobertura" $ do
    it "prints an empty report as fully passing" $ do
      actual <- render []
      withoutTimestamp actual `shouldBe` report "1.0" "0" []

    it "prints each failing line once in ascending order" $ do
      let checks =
            [ CheckFailure
                { code = "DL2003",
                  severity = DLInfoC,
                  message = "test 2",
                  line = 3
                },
              CheckFailure
                { code = "DL2001",
                  severity = DLWarningC,
                  message = "test",
                  line = 1
                },
              CheckFailure
                { code = "DL2002",
                  severity = DLErrorC,
                  message = "same line",
                  line = 3
                }
            ]
      actual <- render checks
      withoutTimestamp actual `shouldBe` report "0.0" "2" [1, 3]


render :: (?noColor :: Bool) => [CheckFailure] -> IO XML.Document
render failures = do
  let results =
        NonEmpty.fromList
          [Result "Dockerfile" mempty (Seq.fromList failures)]
  (output, _) <- capture $ write [] [Cobertura] ?noColor (Just "path/Dockerfile") results
  pure $ XML.parseLBS_ XML.def $ BSC.pack output


withoutTimestamp :: XML.Document -> XML.Document
withoutTimestamp document =
  document
    { XML.documentRoot =
        (XML.documentRoot document)
          { XML.elementAttributes =
              Map.delete "timestamp" $ XML.elementAttributes $ XML.documentRoot document
          }
    }


report :: Text.Text -> Text.Text -> [Int] -> XML.Document
report rate validLines lineNumbers =
  XML.Document
    { XML.documentPrologue = XML.Prologue [] Nothing [],
      XML.documentRoot =
        XML.Element
          { XML.elementName = "coverage",
            XML.elementAttributes =
              Map.fromList
                [ ("line-rate", rate),
                  ("branch-rate", "0.0"),
                  ("lines-covered", "0"),
                  ("lines-valid", validLines),
                  ("branches-covered", "0"),
                  ("branches-valid", "0"),
                  ("complexity", "0.0"),
                  ("version", Text.pack getShortVersion)
                ],
            XML.elementNodes =
              [ XML.NodeElement
                  XML.Element
                    { XML.elementName = "sources",
                      XML.elementAttributes = mempty,
                      XML.elementNodes =
                        [ XML.NodeElement
                            XML.Element
                              { XML.elementName = "source",
                                XML.elementAttributes = mempty,
                                XML.elementNodes = [XML.NodeContent "."]
                              }
                        ]
                    },
                XML.NodeElement
                  XML.Element
                    { XML.elementName = "packages",
                      XML.elementAttributes = mempty,
                      XML.elementNodes =
                        [ XML.NodeElement
                            XML.Element
                              { XML.elementName = "package",
                                XML.elementAttributes =
                                  Map.fromList
                                    [ ("name", "hadolint"),
                                      ("line-rate", rate),
                                      ("branch-rate", "0.0"),
                                      ("complexity", "0.0")
                                    ],
                                XML.elementNodes =
                                  [ XML.NodeElement
                                      XML.Element
                                        { XML.elementName = "classes",
                                          XML.elementAttributes = mempty,
                                          XML.elementNodes = [XML.NodeElement $ classNode rate lineNumbers]
                                        }
                                  ]
                              }
                        ]
                    }
              ]
          },
      XML.documentEpilogue = []
    }


classNode :: Text.Text -> [Int] -> XML.Element
classNode rate lineNumbers =
  XML.Element
    { XML.elementName = "class",
      XML.elementAttributes =
        Map.fromList
          [ ("name", "path/Dockerfile"),
            ("filename", "path/Dockerfile"),
            ("line-rate", rate),
            ("branch-rate", "0.0"),
            ("complexity", "0.0")
          ],
      XML.elementNodes =
        [ XML.NodeElement
            XML.Element
              { XML.elementName = "methods",
                XML.elementAttributes = mempty,
                XML.elementNodes = []
              },
          XML.NodeElement
            XML.Element
              { XML.elementName = "lines",
                XML.elementAttributes = mempty,
                XML.elementNodes = fmap lineNode lineNumbers
              }
        ]
    }


lineNode :: Int -> XML.Node
lineNode number =
  XML.NodeElement
    XML.Element
      { XML.elementName = "line",
        XML.elementAttributes =
          Map.fromList
            [ ("number", Text.pack $ show number),
              ("hits", "0")
            ],
        XML.elementNodes = []
      }
