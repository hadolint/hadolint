import Analyzer
import Parser
import Rules
import Data.List (find)
import Data.Maybe (isJust, fromMaybe)

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

assertChecks ruleName s f = case parseString (s ++ "\n") of
    Left err -> assertFailure $ show err
    Right d  -> f $ findRules (analyze d) ruleName

findRules checks ruleName = filter filterRule checks
    where filterRule (DockerfileCheck rule _) = (name rule) == ruleName
          filterRule (InstructionCheck rule pos) = (name rule) == ruleName

assertOneFailed ruleName s msg = assertChecks ruleName s f
    where f checks = assertEqual msg 1 (length $ failedChecks checks)

assertOneSucceeded ruleName s msg = assertChecks ruleName s f
    where f checks = assertEqual msg 1 (length $ successfulChecks checks)

untagged = TestCase $ assertOneFailed "NoUntagged" "FROM debian" "Should find untagged"

explicit_latest = TestCase $ assertOneFailed "NoLatestTag" "FROM debian:latest" "Should find explicit latest tag"
no_latest = TestCase $ assertOneSucceeded "NoLatestTag" "FROM debian:jessie" "Should find no latest tag"

sudo = TestCase $ assertOneFailed "NoSudo" "RUN sudo apt-get update" "Should find command executed under sudo"
no_sudo_after_program = TestCase $ assertOneSucceeded "NoSudo" "RUN apt-get install sudo" "Sudo should only be applied if it is a program not an argument"
sudo_chained_programs = TestCase $ assertOneFailed "NoSudo" "RUN apt-get update && sudo apt-get install" "Sudo should be detected in chained programs"

invalid_cmd = TestCase $ assertOneFailed "InvalidCmd" "RUN top" "Should find invalid cmd if is command"
no_invalid_cmd_after_program = TestCase $ assertOneSucceeded "InvalidCmd" "RUN apt-get install top" "Should not find invalid cmd if not program"

apt_upgrade = TestCase $ assertOneFailed "NoUpgrade" "RUN apt-get update && apt-get upgrade" "Should find forbidden upgrade command"

apt_get_version_pinning = TestCase $ assertOneFailed "AptGetVersionPinning" "RUN apt-get update && apt-get install python" "Should find unpinned package version"

tests = TestList [ TestLabel "untagged" untagged
                 , TestLabel "explicit_latest" explicit_latest
                 , TestLabel "no_latest" no_latest
                 , TestLabel "no_sudo_after_program" no_sudo_after_program
                 , TestLabel "sudo" sudo
                 , TestLabel "sudo_chained_programs" sudo_chained_programs
                 , TestLabel "invalid_cmd" invalid_cmd
                 , TestLabel "no_invalid_cmd_after_program" no_invalid_cmd_after_program
                 , TestLabel "apt_upgrade" apt_upgrade
                 , TestLabel "apt_get_version_pinning" apt_get_version_pinning
                 ]

main = defaultMain $ hUnitTestToTests tests
