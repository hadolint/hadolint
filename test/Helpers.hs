module Helpers where

import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Aeson hiding (Result)
import Hadolint (Configuration (..), OutputFormat (..), printResults)
import Hadolint.Formatter.Format (Result (..))
import Hadolint.Formatter.TTY (formatCheck)
import Hadolint.Rule (CheckFailure (..), Failures, RuleCode (..))
import Language.Docker.Parser
import Language.Docker.Syntax
import System.IO.Silently
import Test.HUnit hiding (Label)
import Test.Hspec
import qualified Control.Foldl as Foldl
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Hadolint.Process


assertChecks ::
  (HasCallStack, ?config :: Configuration) =>
  Text.Text ->
  (Failures -> IO a) ->
  IO a
assertChecks dockerfile makeAssertions =
  case parseText (dockerfile <> "\n") of
    Left err -> assertFailure $ show err
    Right dockerFile -> makeAssertions $ Hadolint.Process.run ?config dockerFile

assertOnBuildChecks ::
  (HasCallStack, ?config :: Configuration) =>
  Text.Text ->
  (Failures -> IO a) ->
  IO a
assertOnBuildChecks dockerfile makeAssertions =
  case parseText (dockerfile <> "\n") of
    Left err -> assertFailure $ show err
    Right dockerFile -> checkOnBuild dockerFile
  where
    checkOnBuild dockerFile = makeAssertions $ Hadolint.Process.run ?config (fmap wrapInOnBuild dockerFile)
    wrapInOnBuild (InstructionPos (Run args) so li) = InstructionPos (OnBuild (Run args)) so li
    wrapInOnBuild i = i

hasInvalidLines :: Failures -> Bool
hasInvalidLines =
  Foldl.fold
    (Foldl.any (\CheckFailure {line} -> line <= 0))

-- Assert a failed check exists for rule
ruleCatches ::
  (HasCallStack, ?config :: Configuration) =>
  RuleCode ->
  Text.Text ->
  Assertion
ruleCatches expectedCode dockerfile = assertChecks dockerfile f
  where
    f checks = do
      failsWithSome expectedCode checks
      assertBool "Incorrect line number for result" $ not $ hasInvalidLines checks

onBuildRuleCatches ::
  (HasCallStack, ?config :: Configuration) =>
  RuleCode ->
  Text.Text ->
  Assertion
onBuildRuleCatches ruleCode dockerfile = assertOnBuildChecks dockerfile f
  where
    f checks = do
      failsWith 1 ruleCode checks
      assertBool "Incorrect line number for result" $ not $ hasInvalidLines checks

ruleCatchesNot ::
  (HasCallStack, ?config :: Configuration) =>
  RuleCode ->
  Text.Text ->
  Assertion
ruleCatchesNot ruleCode dockerfile = assertChecks dockerfile f
  where
    f = failsWith 0 ruleCode

onBuildRuleCatchesNot ::
  (HasCallStack, ?config :: Configuration) =>
  RuleCode ->
  Text.Text ->
  Assertion
onBuildRuleCatchesNot ruleCode dockerfile = assertOnBuildChecks dockerfile f
  where
    f = failsWith 0 ruleCode

formatChecksNoColor :: Foldable f => f CheckFailure -> Text.Text
formatChecksNoColor = Foldl.fold (Foldl.premap (\c -> formatCheck True "line" c <> "\n") Foldl.mconcat)

failsWithSome :: HasCallStack => RuleCode -> Failures -> Assertion
failsWithSome expectedCode failures =
  when (null matched) $
    assertFailure $ "I was expecting to catch at least one error for " <> show (unRuleCode expectedCode)
  where
    matched = Seq.filter (\CheckFailure {code} -> expectedCode == code) failures

failsWith :: HasCallStack => Int -> RuleCode -> Failures -> Assertion
failsWith times expectedCode failures =
  when (length matched /= times) $
    assertFailure $
      "I was expecting to catch exactly " <> show times <> " error(s) for " <> show (unRuleCode expectedCode) <> ". Found: \n"
        <> (Text.unpack . formatChecksNoColor $ matched)
  where
    matched = Seq.filter (\CheckFailure {code} -> expectedCode == code) failures

failsShellcheck :: HasCallStack => Failures -> Assertion
failsShellcheck checks =
  when (null matched) $
    assertFailure
      "I was expecting to catch at least one error with shellcheck"
  where
    matched = Seq.filter (\CheckFailure {code = RuleCode rc} -> "SC" `Text.isPrefixOf` rc) checks

passesShellcheck :: HasCallStack => Failures -> Assertion
passesShellcheck checks =
  unless (null matched) $
    assertFailure $
      "I was expecting to catch no errors with shellcheck. Found: \n"
        <> (Text.unpack . formatChecksNoColor $ matched)
  where
    matched = Seq.filter (\CheckFailure {code = RuleCode rc} -> "SC" `Text.isPrefixOf` rc) checks


assertFormatter ::
  (HasCallStack, ?noColor :: Bool) =>
  OutputFormat ->
  [CheckFailure] ->
  String ->
  Assertion
assertFormatter formatter failures expectation = do
  let results =
        NonEmpty.fromList [Result "<string>" mempty (Seq.fromList failures)]
  (cap, _) <- capture
                (printResults formatter ?noColor (Just "<string>") results)
  cap `shouldBe` expectation

assertFormatterJson
  :: (HasCallStack, ?noColor :: Bool) =>
  OutputFormat ->
  [CheckFailure] ->
  Value ->
  Assertion
assertFormatterJson formatter failures expectation = do
  let results =
        NonEmpty.fromList [ Result "<string>" mempty (Seq.fromList failures) ]
  (cap, _) <- capture
                (printResults formatter ?noColor (Just "<string>") results)
  decode (BSC.pack cap) `shouldBe` Just expectation
