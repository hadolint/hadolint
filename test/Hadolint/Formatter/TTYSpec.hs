module Hadolint.Formatter.TTYSpec (spec) where

import Helpers
import Data.List.NonEmpty as NonEmpty
import Hadolint (OutputFormat (..), printResults)
import Hadolint.Formatter.Format (Result (..))
import Hadolint.Rule (CheckFailure (..), DLSeverity (..))
import Test.Hspec
import qualified Data.Sequence as Seq


spec :: SpecWith ()
spec = do
  let ?noColor = True
  describe "Formatter: TTY" $ do
    it "print empty results" $ do
      let results = NonEmpty.fromList [Result "<string>" mempty Seq.empty]
      printResults TTY True (Just "<string>") results `shouldReturn` ()

    it "print some result: no colors" $ do
      let checkFails = [ CheckFailure
                          { code = "DL2001",
                            severity = DLInfoC,
                            message = "test",
                            line = 1
                          }
                       ]
          expectation = unlines
                          [ "<string>:1 DL2001 info: test"
                          ]
      assertFormatter TTY checkFails expectation

    it "print some result: with colors" $ do
      let ?noColor = False
      let checkFails = [ CheckFailure
                          { code = "DL2001",
                            severity = DLInfoC,
                            message = "test",
                            line = 1
                          }
                       ]
          expectation = unlines
                          [ "<string>:1 DL2001 \ESC[92minfo\ESC[0m: test"
                          ]
      assertFormatter TTY checkFails expectation

    it "print multiple results: no colors" $ do
      let checkFails = [ CheckFailure
                          { code = "DL2001",
                            severity = DLInfoC,
                            message = "test",
                            line = 1
                          },
                         CheckFailure
                          { code = "DL2002",
                            severity = DLWarningC,
                            message = "foo",
                            line = 3
                          }
                       ]
          expectation = unlines
                          [ "<string>:1 DL2001 info: test",
                            "<string>:3 DL2002 warning: foo"
                          ]
      assertFormatter TTY checkFails expectation

    it "print multiple results: with colors" $ do
      let ?noColor = False
      let checkFails = [ CheckFailure
                          { code = "DL2001",
                            severity = DLInfoC,
                            message = "test",
                            line = 1
                          },
                         CheckFailure
                          { code = "DL2002",
                            severity = DLWarningC,
                            message = "foo",
                            line = 3
                          }
                       ]
          expectation = unlines
                          [ "<string>:1 DL2001 \ESC[92minfo\ESC[0m: test",
                            "<string>:3 DL2002 \ESC[1m\ESC[93mwarning\ESC[0m:\
                            \ foo"
                          ]
      assertFormatter TTY checkFails expectation
