module Hadolint.Formatter.GnuSpec (spec) where


import Helpers
import Hadolint
  ( CheckFailure (..),
    DLSeverity (..),
    OutputFormat (..),
  )
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?noColor = False

  describe "Formatter: Gnu" $ do
    it "print empty results" $ do
      let checkFails = []
          expectation = ""
      assertFormatter Gnu checkFails expectation

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
            "hadolint:<string>:1: DL2001 warning: test\n"
      assertFormatter Gnu checkFails expectation

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
            "hadolint:<string>:1: DL2001 warning: test\n\
            \hadolint:<string>:3: DL2003 info: test 2\n"
      assertFormatter Gnu checkFails expectation
