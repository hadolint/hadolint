module Hadolint.Formatter.JUnitSpec ( spec ) where

import Helpers
import Hadolint
  ( CheckFailure (..),
    DLSeverity (..),
    OutputFormat (..),
    getShortVersion,
  )
import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Text.XML as XML


spec :: SpecWith ()
spec = do
  time <- runIO Time.getCurrentTime

  let runID = Text.pack $ Time.formatTime Time.defaultTimeLocale "%Y%m%d_%H%M%S" time
  let runName = Text.pack $ Time.formatTime Time.defaultTimeLocale "Hadolint run at %Y-%m-%d %H:%M:%S" time
  let providerName = Text.pack $ "Hadolint " <> getShortVersion

  let ?noColor = True

  describe "Formatter: JUnit" $ do
    it "print empty results" $ do
      let checkFails = []
          expectation =
            XML.Document
              { documentPrologue = XML.Prologue [] Nothing [],
                documentRoot =
                  XML.Element
                    { elementName = "testsuites",
                      elementAttributes =
                        Map.fromList
                          [ ("id", runID),
                            ("name", runName),
                            ("time", "0.001")
                          ],
                      elementNodes =
                        [ XML.NodeElement XML.Element
                            { elementName = "testsuite",
                              elementAttributes =
                                Map.fromList
                                  [ ("id", "hadolint"),
                                    ("name", providerName),
                                    ("time", "0.001"),
                                    ("failures", "0"),
                                    ("errors", "0")
                                  ],
                              elementNodes = []
                            }
                        ]
                    },
                documentEpilogue = []
              }
      assertFormatterXML JUnit checkFails expectation

    it "print one failed rule" $ do
      let checkFails =
            [ CheckFailure
                { code = "DL2001",
                  severity = DLWarningC,
                  message = "test",
                  line = 1
                }
            ]
          expectation =
            XML.Document
              { documentPrologue = XML.Prologue [] Nothing [],
                documentRoot =
                  XML.Element
                    { elementName = "testsuites",
                      elementAttributes =
                        Map.fromList
                          [ ("id", runID),
                            ("name", runName),
                            ("time", "0.001")
                          ],
                      elementNodes =
                        [ XML.NodeElement XML.Element
                            { elementName = "testsuite",
                              elementAttributes =
                                Map.fromList
                                  [ ("id", "hadolint"),
                                    ("name", providerName),
                                    ("time", "0.001"),
                                    ("failures", "1"),
                                    ("errors", "0")
                                  ],
                              elementNodes =
                                [ XML.NodeElement XML.Element
                                    { elementName = "testcase",
                                      elementAttributes =
                                        Map.fromList
                                          [ ("id", "hadolint.rule.DL2001"),
                                            ("time", "0.001")
                                          ],
                                      elementNodes =
                                        [ XML.NodeElement XML.Element
                                            { elementName = "failure",
                                              elementAttributes =
                                                Map.fromList
                                                  [ ("type", "warning"),
                                                    ("message", "test"),
                                                    ("id", "DL2001")
                                                  ],
                                              elementNodes =
                                                [ XML.NodeContent $
                                                    Text.unlines
                                                      [ "File: <string>",
                                                        "Line: 1",
                                                        "Category: Hadolint - Dockerfile Static Analysis",
                                                        "warning: test"
                                                      ]
                                                ]
                                            }
                                        ]
                                    }
                                ]
                            }
                        ]
                    },
                documentEpilogue = []
              }
      assertFormatterXML JUnit checkFails expectation

    it "print multiple failed rules" $ do
      let checkFails =
            [ CheckFailure
                { code = "DL2001",
                  severity = DLWarningC,
                  message = "test",
                  line = 1
                },
              CheckFailure
                { code = "DL2003",
                  severity = DLInfoC,
                  message = "test 2",
                  line = 3
                }
            ]
          expectation =
            XML.Document
              { documentPrologue = XML.Prologue [] Nothing [],
                documentRoot =
                  XML.Element
                    { elementName = "testsuites",
                      elementAttributes =
                        Map.fromList
                          [ ("id", runID),
                            ("name", runName),
                            ("time", "0.001")
                          ],
                      elementNodes =
                        [ XML.NodeElement XML.Element
                            { elementName = "testsuite",
                              elementAttributes =
                                Map.fromList
                                  [ ("id", "hadolint"),
                                    ("name", "Hadolint " <> Text.pack getShortVersion),
                                    ("time", "0.001"),
                                    ("failures", "2"),
                                    ("errors", "0")
                                  ],
                              elementNodes =
                                [ XML.NodeElement XML.Element
                                    { elementName = "testcase",
                                      elementAttributes =
                                        Map.fromList
                                          [ ("id", "hadolint.rule.DL2001"),
                                            ("time", "0.001")
                                          ],
                                      elementNodes =
                                        [ XML.NodeElement XML.Element
                                            { elementName = "failure",
                                              elementAttributes =
                                                Map.fromList
                                                  [ ("type", "warning"),
                                                    ("message", "test"),
                                                    ("id", "DL2001")
                                                  ],
                                              elementNodes =
                                                [ XML.NodeContent $
                                                    Text.unlines
                                                      [ "File: <string>",
                                                        "Line: 1",
                                                        "Category: Hadolint - Dockerfile Static Analysis",
                                                        "warning: test"
                                                      ]
                                                ]
                                            }
                                        ]
                                    },
                                  XML.NodeElement XML.Element
                                    { elementName = "testcase",
                                      elementAttributes =
                                        Map.fromList
                                          [ ("id", "hadolint.rule.DL2003"),
                                            ("time", "0.001")
                                          ],
                                      elementNodes =
                                        [ XML.NodeElement XML.Element
                                            { elementName = "failure",
                                              elementAttributes =
                                                Map.fromList
                                                  [ ("type", "info"),
                                                    ("message", "test 2"),
                                                    ("id", "DL2003")
                                                  ],
                                              elementNodes =
                                                [ XML.NodeContent $
                                                    Text.unlines
                                                      [ "File: <string>",
                                                        "Line: 3",
                                                        "Category: Hadolint - Dockerfile Static Analysis",
                                                        "info: test 2"
                                                      ]
                                                ]
                                            }
                                        ]
                                    }
                                ]
                            }
                        ]
                    },
                documentEpilogue = []
              }
      assertFormatterXML JUnit checkFails expectation
