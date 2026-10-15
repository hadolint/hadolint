module Hadolint.Formatter.CodeQualitySpec ( spec ) where


import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import Helpers
import Hadolint
  ( CheckFailure (..),
    DLSeverity (..),
    OutputFormat (..),
  )
import Hadolint.Formatter (write)
import Hadolint.Formatter.Format (Result (..))
import System.IO.Silently (capture)
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?noColor = True
  let mkIssue rule severity msg line path fingerprint =
        object
          [ "fingerprint" .= String fingerprint,
            "check_name" .= String rule,
            "description" .= String msg,
            "severity" .= String severity,
            "location" .= object [ "path" .= String path, "lines" .= object [ "begin" .= Number line ] ]
          ]

  describe "Formatter: GitLab Code Quality JSON" $ do
    it "print empty results" $ do
      let checkFails = []
          expectation = Array []
      assertFormatterJson GitLabCodeQualityJson checkFails expectation

    it "print several issues with default path" $ do
      let checkFails =
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
          results = NonEmpty.fromList [Result "Dockerfile" mempty (Seq.fromList checkFails)]
          customPath = Nothing
          expectation =
            Array
              [ mkIssue "DL3000" "blocker" "error msg" 1 "Dockerfile" "8c24ad9492ded635f713f3c78923d5510b111f40",
                mkIssue "DL3001" "major" "warning msg" 2 "Dockerfile" "87776f32f22334316c9631f286591c50bc9e94f8",
                mkIssue "DL3009" "minor" "info msg" 3 "Dockerfile" "740874fcb748f021198f3f562f5f1408d319cc1a",
                mkIssue "DL3015" "info" "style msg" 4 "Dockerfile" "cb499eec1a275148cb6f736b4086a3adea064b89"
              ]
      (cap, _) <- capture (write [] [GitLabCodeQualityJson] ?noColor customPath results)
      decode (BSC.pack cap) `shouldBe` Just expectation

    it "print several issues with custom path" $ do
      let checkFails =
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
          results = NonEmpty.fromList [Result "<string>" mempty (Seq.fromList checkFails)]
          customPath = Just "path/to/custom/Dockerfile"
          expectation =
            Array
              [ mkIssue "DL3000" "blocker" "error msg" 1 "path/to/custom/Dockerfile" "146a5e52d3679c0a06b6f9194a65dce5e508daa3",
                mkIssue "DL3001" "major" "warning msg" 2 "path/to/custom/Dockerfile" "5bf0f9deb716d98f5be8989c6e7050ba0e8af399",
                mkIssue "DL3009" "minor" "info msg" 3 "path/to/custom/Dockerfile" "e9079fa0a99b607182c15dca9b65932dfd45c627",
                mkIssue "DL3015" "info" "style msg" 4 "path/to/custom/Dockerfile" "07d17ce9a67fd5cd2160ded51848c7e9583c52fe"
              ]
      (cap, _) <- capture (write [] [GitLabCodeQualityJson] ?noColor customPath results)
      decode (BSC.pack cap) `shouldBe` Just expectation
