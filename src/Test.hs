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

ruleCatchesNot :: Rule -> String -> Assertion
ruleCatchesNot rule s = assertChecks rule s f
    where f checks = assertEqual ("Check of rule " ++ name rule ++ " found") 0 $ length checks

tests = test [ "untagged" ~: ruleCatches noUntagged "FROM debian"
             , "explicit latest" ~: ruleCatches noLatestTag "FROM debian:latest"
             , "explicit tagged" ~: ruleCatchesNot noLatestTag "FROM debian:jessie"
             , "sudo" ~: ruleCatches noSudo "RUN sudo apt-get update"
             , "install sudo" ~: ruleCatchesNot noSudo "RUN apt-get install sudo"
             , "sudo chained programs" ~: ruleCatches noSudo "RUN apt-get update && sudo apt-get install"
             , "invalid cmd" ~: ruleCatches invalidCmd "RUN top"
             , "install ssh" ~: ruleCatchesNot invalidCmd "RUN apt-get install ssh"
             , "apt upgrade" ~: ruleCatches noUpgrade "RUN apt-get update && apt-get upgrade"
             , "apt-get version pinning" ~: ruleCatches aptGetVersionPinned "RUN apt-get update && apt-get install python"
             , "apt-get no cleanup" ~: ruleCatches aptGetCleanup "RUN apt-get update && apt-get install python"
             , "apt-get cleanup" ~: ruleCatchesNot aptGetCleanup "RUN apt-get update && apt-get install python && rm -rf /var/lib/apt/lists/*"
             , "use add" ~: ruleCatches useAdd "COPY packaged-app.tar /usr/src/app"
             , "use not add" ~: ruleCatchesNot useAdd "COPY package.json /usr/src/app"
             ]

main = defaultMain $ hUnitTestToTests tests
