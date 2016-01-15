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
    where f checks = assertEqual "No check for rule found" 1 $ length checks

ruleCatchesNot :: Rule -> String -> Assertion
ruleCatchesNot rule s = assertChecks rule s f
    where f checks = assertEqual "Found check of rule" 0 $ length checks

tests = test [ "untagged" ~: ruleCatches noUntagged "FROM debian"
             , "explicit latest" ~: ruleCatches noLatestTag "FROM debian:latest"
             , "explicit tagged" ~: ruleCatchesNot noLatestTag "FROM debian:jessie"
             , "sudo" ~: ruleCatches noSudo "RUN sudo apt-get update"
             , "no root" ~: ruleCatches noRootUser "USER root"
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
             , "invalid port" ~: ruleCatches invalidPort "EXPOSE 80000"
             , "valid port" ~: ruleCatchesNot invalidPort "EXPOSE 60000"
             , "maintainer address" ~: ruleCatches maintainerAddress "MAINTAINER Lukas"
             , "maintainer uri" ~: ruleCatchesNot maintainerAddress "MAINTAINER Lukas <me@lukasmartinelli.ch>"
             , "maintainer mail" ~: ruleCatchesNot maintainerAddress "MAINTAINER http://lukasmartinelli.ch"
             , "pip requirements" ~: ruleCatchesNot pipVersionPinned "RUN pip install -r requirements.txt"
             , "pip version not pinned" ~: ruleCatches pipVersionPinned "RUN pip install MySQL_python"
             , "pip version pinned" ~: ruleCatchesNot pipVersionPinned "RUN pip install MySQL_python==1.2.2"
             , "apt-get auto yes" ~: ruleCatches aptGetYes "RUN apt-get install python"
             , "apt-get yes shortflag" ~: ruleCatchesNot aptGetYes "RUN apt-get install -yq python"
             , "apt-get yes different pos" ~: ruleCatchesNot aptGetYes "RUN apt-get install -y python"
             , "apt-get with auto yes" ~: ruleCatchesNot aptGetYes "RUN apt-get -y install python"
             , "apt-get with auto expanded yes" ~: ruleCatchesNot aptGetYes "RUN apt-get --yes install python"
             , "apt-get install recommends" ~: ruleCatchesNot aptGetNoRecommends "RUN apt-get install --no-install-recommends python"
             , "apt-get no install recommends" ~: ruleCatches aptGetNoRecommends "RUN apt-get install python"
             , "apt-get no install recommends" ~: ruleCatches aptGetNoRecommends "RUN apt-get -y install python"
             , "apt-get version" ~: ruleCatchesNot aptGetVersionPinned "RUN apt-get install -y python=1.2.2"
             , "apt-get pinned" ~: ruleCatchesNot aptGetVersionPinned "RUN apt-get -y --no-install-recommends install nodejs=0.10"
             , "apt-get pinned chained" ~: ruleCatchesNot aptGetVersionPinned $ unlines [
                 "RUN apt-get update \\",
                 " && apt-get -y --no-install-recommends install nodejs=0.10 \\",
                 " && rm -rf /var/lib/apt/lists/*"
                 ]
             , "has maintainer" ~: ruleCatchesNot hasMaintainer "FROM debian\nMAINTAINER Lukas"
             , "has maintainer first" ~: ruleCatchesNot hasMaintainer "MAINTAINER Lukas\nFROM DEBIAN"
             , "has no maintainer" ~: ruleCatches hasMaintainer "FROM debian"
             , "using add" ~: ruleCatches copyInsteadAdd "ADD file /usr/src/app/"
             , "add is ok for archive" ~: ruleCatchesNot copyInsteadAdd "ADD file.tar /usr/src/app/"
             , "add is ok for url" ~: ruleCatchesNot copyInsteadAdd "ADD http://file.com /usr/src/app/"
             , "many cmds" ~: ruleCatches multipleCmds "CMD /bin/true\nCMD /bin/true"
             , "single cmd" ~: ruleCatchesNot multipleCmds "CMD /bin/true"
             , "no cmd" ~: ruleCatchesNot multipleEntrypoints "FROM busybox"
             , "many entries" ~: ruleCatches multipleEntrypoints "ENTRYPOINT /bin/true\nENTRYPOINT /bin/true"
             , "single entry" ~: ruleCatchesNot multipleEntrypoints "ENTRYPOINT /bin/true"
             , "no entry" ~: ruleCatchesNot multipleEntrypoints "FROM busybox"
             ]

main = defaultMain $ hUnitTestToTests tests
