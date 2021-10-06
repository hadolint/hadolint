module Hadolint.Formatter.TTYSpec (spec) where

import Data.List.NonEmpty as NonEmpty
import Hadolint (OutputFormat (..), printResults)
import Hadolint.Formatter.Format (Result (..))
import Hadolint.Rule (CheckFailure (..), DLSeverity (..))
import System.IO.Silently
import Test.Hspec
import qualified Data.Sequence as Seq


spec :: SpecWith ()
spec = do
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
          results = NonEmpty.fromList
                      [Result "<string>" mempty (Seq.fromList checkFails)]
          expectation = unlines
                          [ "<string>:1 DL2001 info: test"
                          ]
      (cap, _) <- capture (printResults TTY True (Just "<string>") results)
      cap `shouldBe` expectation

    it "print some result: with colors" $ do
      let checkFails = [ CheckFailure
                          { code = "DL2001",
                            severity = DLInfoC,
                            message = "test",
                            line = 1
                          }
                       ]
          results = NonEmpty.fromList
                      [Result "<string>" mempty (Seq.fromList checkFails)]
          expectation = unlines
                          [ "<string>:1 DL2001 \ESC[92minfo\ESC[0m: test"
                          ]
      (cap, _) <- capture (printResults TTY False (Just "<string>") results)
      cap `shouldBe` expectation

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
          results = NonEmpty.fromList
                      [Result "<string>" mempty (Seq.fromList checkFails)]
          expectation = unlines
                          [ "<string>:1 DL2001 info: test",
                            "<string>:3 DL2002 warning: foo"
                          ]
      (cap, _) <- capture (printResults TTY True (Just "<string>") results)
      cap `shouldBe` expectation

    it "print multiple results: with colors" $ do
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
          results = NonEmpty.fromList
                      [Result "<string>" mempty (Seq.fromList checkFails)]
          expectation = unlines
                          [ "<string>:1 DL2001 \ESC[92minfo\ESC[0m: test",
                            "<string>:3 DL2002 \ESC[1m\ESC[93mwarning\ESC[0m:\
                            \ foo"
                          ]
      (cap, _) <- capture (printResults TTY False (Just "<string>") results)
      cap `shouldBe` expectation
