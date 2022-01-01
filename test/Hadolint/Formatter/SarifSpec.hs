module Hadolint.Formatter.SarifSpec (spec) where

import Data.Aeson
import Helpers
import Hadolint
  ( CheckFailure (..),
    DLSeverity (..),
    OutputFormat (..),
    getShortVersion,
  )
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?noColor = True
  let version = String "2.1.0"
      schema = String "http://json.schemastore.org/sarif-2.1.0"
      defSrcLang = String "dockerfile"
      tool =
        Object
          [ "driver" .=
              Object
                [ "name" .= String "Hadolint",
                  "fullName" .= String "Haskell Dockerfile Linter",
                  "downloadUri" .=
                    String "https://github.com/hadolint/hadolint",
                  "version" .= getShortVersion,
                  "shortDescription" .=
                    Object
                      [ "text" .= String "Dockerfile linter, validate inline\
                                         \ bash, written in Haskell"
                      ]
                ]
          ]

  describe "Formatter: Sarif" $ do
    it "print empty results" $ do
      let checkFails = []
          expectation =
            Object
              [ "version" .= version,
                "$schema" .= schema,
                "runs" .=
                  Array
                    [ Object
                        [ "tool" .= tool,
                          "results" .= Array [],
                          "defaultSourceLanguage" .= defSrcLang
                        ]
                    ]
              ]
      assertFormatterJson Sarif checkFails expectation

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
            Object
              [ "version" .= version,
                "$schema" .= schema,
                "runs" .=
                  Array
                    [ Object
                        [ "tool" .= tool,
                          "results" .=
                            Array
                              [ Object
                                  [ "level" .= String "warning",
                                    "locations" .=
                                      Array
                                        [ Object
                                            [ "physicalLocation" .=
                                                Object
                                                  [ "artifactLocation" .=
                                                      Object
                                                        [ "uri" .=
                                                            String "<string>"
                                                        ],
                                                    "region" .=
                                                      Object
                                                        [ "startLine" .=
                                                            Number 1.0,
                                                          "endLine" .=
                                                            Number 1.0,
                                                          "startColumn" .=
                                                            Number 1.0,
                                                          "endColumn" .=
                                                            Number 1.0,
                                                          "sourceLanguage" .=
                                                            String "dockerfile"
                                                        ]
                                                  ]
                                            ]
                                        ],
                                    "message" .=
                                      Object
                                        [ "text" .= String "test"
                                        ],
                                    "ruleId" .= String "DL2001"
                                  ]
                              ],
                          "defaultSourceLanguage" .= defSrcLang
                        ]
                    ]
              ]
      assertFormatterJson Sarif checkFails expectation

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
            Object
              [ "version" .= version,
                "$schema" .= schema,
                "runs" .=
                  Array
                    [ Object
                        [ "tool" .= tool,
                          "results" .=
                            Array
                              [ Object
                                  [ "level" .= String "warning",
                                    "locations" .=
                                      Array
                                        [ Object
                                            [ "physicalLocation" .=
                                                Object
                                                  [ "artifactLocation" .=
                                                      Object
                                                        [ "uri" .=
                                                            String "<string>"
                                                        ],
                                                    "region" .=
                                                      Object
                                                        [ "startLine" .=
                                                            Number 1.0,
                                                          "endLine" .=
                                                            Number 1.0,
                                                          "startColumn" .=
                                                            Number 1.0,
                                                          "endColumn" .=
                                                            Number 1.0,
                                                          "sourceLanguage" .=
                                                            String "dockerfile"
                                                        ]
                                                  ]
                                            ]
                                        ],
                                    "message" .=
                                      Object
                                        [ "text" .= String "test"
                                        ],
                                    "ruleId" .= String "DL2001"
                                  ],
                                Object
                                  [ "level" .= String "note",
                                    "locations" .=
                                      Array
                                        [ Object
                                            [ "physicalLocation" .=
                                                Object
                                                  [ "artifactLocation" .=
                                                      Object
                                                        [ "uri" .=
                                                            String "<string>"
                                                        ],
                                                    "region" .=
                                                      Object
                                                        [ "startLine" .=
                                                            Number 3.0,
                                                          "endLine" .=
                                                            Number 3.0,
                                                          "startColumn" .=
                                                            Number 1.0,
                                                          "endColumn" .=
                                                            Number 1.0,
                                                          "sourceLanguage" .=
                                                            String "dockerfile"
                                                        ]
                                                  ]
                                            ]
                                        ],
                                    "message" .=
                                      Object
                                        [ "text" .= String "test 2"
                                        ],
                                    "ruleId" .= String "DL2003"
                                  ]
                              ],
                          "defaultSourceLanguage" .= defSrcLang
                        ]
                    ]
              ]
      assertFormatterJson Sarif checkFails expectation
