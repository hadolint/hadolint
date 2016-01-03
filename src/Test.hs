import Parser
import Rules
import Data.List (find)
import Data.Maybe (isJust, fromMaybe)

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

assertChecks rule s f = case parseString (s ++ "\n") of
    Left err -> assertFailure $ show err
    Right dockerfile  -> f $ analyze [rule] dockerfile

-- Assert a failed check exists for rule
ruleCatches :: Rule -> String -> Assertion
ruleCatches rule s = assertChecks rule s f
    where f checks = assertEqual ("No check of rule " ++ name rule ++ " found") 1 $ length checks

tests = test [ "untagged" ~: ruleCatches noUntagged "FROM debian"
             , "explicit latest" ~: ruleCatches noLatestTag "FROM debian:latest"
             , "sudo" ~: ruleCatches noSudo "RUN sudo apt-get update"
             , "sudo chained programs" ~: ruleCatches noSudo "RUN apt-get update && sudo apt-get install"
             , "invalid cmd" ~: ruleCatches invalidCmd "RUN top"
             , "apt upgrade" ~: ruleCatches noUpgrade "RUN apt-get update && apt-get upgrade"
             , "apt-get version pinning" ~: ruleCatches aptGetVersionPinned "RUN apt-get update && apt-get install python"
             ]

main = defaultMain $ hUnitTestToTests tests
