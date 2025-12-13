module Hadolint.Formatter.SonarQubeSpec (spec) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import Hadolint.Formatter (printResults)
import Hadolint.Formatter.Format (Result (..))
import Helpers
import Hadolint
  ( CheckFailure (..),
    DLSeverity (..),
    OutputFormat (..),
  )
import System.IO.Silently (capture)
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?noColor = True
  let mkIssue ruleId severity issueType msg lineNum =
        Object
          [ "engineId" .= String "Hadolint",
            "ruleId" .= String ruleId,
            "severity" .= String severity,
            "type" .= String issueType,
            "primaryLocation" .=
              Object
                [ "message" .= String msg,
                  "filePath" .= String "<string>",
                  "textRange" .=
                    Object
                      [ "startLine" .= Number lineNum,
                        "endLine" .= Number lineNum,
                        "startColumn" .= Number 0.0,
                        "endColumn" .= Number 1.0
                      ]
                ]
          ]

  describe "Formatter: SonarQube" $ do
    it "print empty results" $ do
      assertFormatterJson SonarQube [] (Object ["issues" .= Array []])

    it "print all severity levels with correct mapping" $ do
      let checks =
            [ CheckFailure
                { code = "DL3000",
                  severity = DLErrorC,
                  message = "error msg",
                  line = 1
                },
              CheckFailure
                { code = "DL3001",
                  severity = DLWarningC,
                  message = "warning msg",
                  line = 2
                },
              CheckFailure
                { code = "DL3009",
                  severity = DLInfoC,
                  message = "info msg",
                  line = 3
                },
              CheckFailure
                { code = "DL3015",
                  severity = DLStyleC,
                  message = "style msg",
                  line = 4
                }
            ]
          expected =
            Object
              [ "issues" .=
                  Array
                    [ mkIssue "DL3000" "CRITICAL" "BUG" "error msg" 1.0,
                      mkIssue "DL3001" "MAJOR" "CODE_SMELL" "warning msg" 2.0,
                      mkIssue "DL3009" "MINOR" "CODE_SMELL" "info msg" 3.0,
                      mkIssue "DL3015" "INFO" "CODE_SMELL" "style msg" 4.0
                    ]
              ]
      assertFormatterJson SonarQube checks expected

    it "use custom file path from --file-path-in-report" $ do
      let checks =
            [ CheckFailure
                { code = "DL3000",
                  severity = DLErrorC,
                  message = "error msg",
                  line = 1
                }
            ]
          results = NonEmpty.fromList [Result "<string>" mempty (Seq.fromList checks)]
          customPath = Just "path/to/custom/Dockerfile"
          mkCustomIssue path =
            Object
              [ "engineId" .= String "Hadolint",
                "ruleId" .= String "DL3000",
                "severity" .= String "CRITICAL",
                "type" .= String "BUG",
                "primaryLocation" .=
                  Object
                    [ "message" .= String "error msg",
                      "filePath" .= String path,
                      "textRange" .=
                        Object
                          [ "startLine" .= Number 1.0,
                            "endLine" .= Number 1.0,
                            "startColumn" .= Number 0.0,
                            "endColumn" .= Number 1.0
                          ]
                    ]
              ]
          expected = Object ["issues" .= Array [mkCustomIssue "path/to/custom/Dockerfile"]]
      (cap, _) <- capture (printResults SonarQube ?noColor customPath results)
      decode (BSC.pack cap) `shouldBe` Just expected
