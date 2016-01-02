import Analyzer
import Parser
import Rules
import Data.List (find)
import Data.Maybe (isJust, fromMaybe)

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

assertChecks ruleName s f = case parseString s of
    Left err -> assertFailure $ show err
    Right d  -> f $ findRules (analyze d) ruleName

findRules checks ruleName = filter filterRule checks
    where filterRule (DockerfileCheck rule _) = (name rule) == ruleName
          filterRule (InstructionCheck rule pos) = (name rule) == ruleName

assertOneFailed ruleName s msg = assertChecks ruleName s f
    where f checks = assertEqual msg 1 (length $ failedChecks checks)

assertOneSucceeded ruleName s msg = assertChecks ruleName s f
    where f checks = assertEqual msg 1 (length $ successfulChecks checks)

implicit_latest = TestCase $ assertOneFailed "NoLatestTag" "FROM debian" "Should find implicit latest tag"
explicit_latest = TestCase $ assertOneFailed "NoLatestTag" "FROM debian:latest" "Should find explicit latest tag"
no_latest = TestCase $ assertOneSucceeded "NoLatestTag" "FROM debian:jessie" "Should find no latest tag"

tests = TestList [ TestLabel "implicit_latest" implicit_latest
                 , TestLabel "explicit_latest" explicit_latest
                 , TestLabel "no_latest" no_latest
                 ]

main = defaultMain $ hUnitTestToTests tests
