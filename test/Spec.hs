import Test.HUnit hiding (Label)
import Test.Hspec

import Hadolint.Formatter.TTY (formatError)
import Hadolint.Rules

import Language.Docker.Parser

import Data.List (find)
import Data.Maybe (fromMaybe, isJust)

main :: IO ()
main =
    hspec $ do
        describe "FROM rules" $ do
            it "no untagged" $ ruleCatches noUntagged "FROM debian"
            it "no untagged with name" $ ruleCatches noUntagged "FROM debian AS builder"
            it "explicit latest" $ ruleCatches noLatestTag "FROM debian:latest"
            it "explicit latest with name" $ ruleCatches noLatestTag "FROM debian:latest AS builder"
            it "explicit tagged" $ ruleCatchesNot noLatestTag "FROM debian:jessie"
            it "explicit SHA" $
                ruleCatchesNot noLatestTag
                    "FROM debian@sha256:\
                    \7959ed6f7e35f8b1aaa06d1d8259d4ee25aa85a086d5c125480c333183f9deeb"
            it "explicit tagged with name" $
              ruleCatchesNot noLatestTag "FROM debian:jessie AS builder"
            it "local aliases are OK to be untagged" $
                let dockerfile =
                        [ "FROM golang:1.9.3-alpine3.7 AS build"
                        , "RUN foo"
                        , "FROM build as unit-test"
                        , "RUN bar"
                        , "FROM alpine:3.7"
                        , "RUN baz"
                        ]
                in ruleCatchesNot noUntagged $ unlines dockerfile
            it "other untagged cases are not ok" $
                let dockerfile =
                        [ "FROM golang:1.9.3-alpine3.7 AS build"
                        , "RUN foo"
                        , "FROM node as unit-test"
                        , "RUN bar"
                        , "FROM alpine:3.7"
                        , "RUN baz"
                        ]
                in ruleCatches noUntagged $ unlines dockerfile
        --
        describe "no root or sudo rules" $ do
            it "sudo" $ ruleCatches noSudo "RUN sudo apt-get update"
            it "no root" $ ruleCatches noRootUser "USER root"
            it "no root" $ ruleCatchesNot noRootUser "USER foo"
            it "no root UID" $ ruleCatches noRootUser "USER 0"
            it "no root:root" $ ruleCatches noRootUser "USER root:root"
            it "no root UID:GID" $ ruleCatches noRootUser "USER 0:0"
            it "install sudo" $ ruleCatchesNot noSudo "RUN apt-get install sudo"
            it "sudo chained programs" $
                ruleCatches noSudo "RUN apt-get update && sudo apt-get install"
        --
        describe "invalid CMD rules" $ do
            it "invalid cmd" $ ruleCatches invalidCmd "RUN top"
            it "install ssh" $ ruleCatchesNot invalidCmd "RUN apt-get install ssh"
        --
        describe "apt-get rules" $ do
            it "apt upgrade" $ ruleCatches noAptGetUpgrade "RUN apt-get update && apt-get upgrade"
            it "apt-get version pinning" $
                ruleCatches aptGetVersionPinned "RUN apt-get update && apt-get install python"
            it "apt-get no cleanup" $
                ruleCatches aptGetCleanup "RUN apt-get update && apt-get install python"
            it "apt-get cleanup" $
                ruleCatchesNot
                    aptGetCleanup
                    "RUN apt-get update && apt-get install python && rm -rf /var/lib/apt/lists/*"
            it "apt-get pinned chained" $
                let dockerfile =
                        [ "RUN apt-get update \\"
                        , " && apt-get -y --no-install-recommends install nodejs=0.10 \\"
                        , " && rm -rf /var/lib/apt/lists/*"
                        ]
                in ruleCatchesNot aptGetVersionPinned $ unlines dockerfile
            it "apt-get pinned regression" $
                let dockerfile =
                        [ "RUN apt-get update && apt-get install --no-install-recommends -y \\"
                        , "python-demjson=2.2.2* \\"
                        , "wget=1.16.1* \\"
                        , "git=1:2.5.0* \\"
                        , "ruby=1:2.1.*"
                        ]
                in ruleCatchesNot aptGetVersionPinned $ unlines dockerfile
            it "has deprecated maintainer" $
                ruleCatches hasNoMaintainer "FROM busybox\nMAINTAINER hudu@mail.com"
        --
        describe "apk add rules" $ do
            it "apk upgrade" $ ruleCatches noApkUpgrade "RUN apk update && apk upgrade"
            it "apk add version pinning single" $ ruleCatches apkAddVersionPinned "RUN apk add flex"
            it "apk add no version pinning single" $
                ruleCatchesNot apkAddVersionPinned "RUN apk add flex=2.6.4-r1"
            it "apk add version pinned chained" $
                let dockerfile =
                        [ "RUN apk add --no-cache flex=2.6.4-r1 \\"
                        , " && pip install -r requirements.txt"
                        ]
                in ruleCatchesNot apkAddVersionPinned $ unlines dockerfile
            it "apk add version pinned regression" $
                let dockerfile =
                        [ "RUN apk add --no-cache \\"
                        , "flex=2.6.4-r1 \\"
                        , "libffi=3.2.1-r3 \\"
                        , "python2=2.7.13-r1 \\"
                        , "libbz2=1.0.6-r5"
                        ]
                in ruleCatchesNot apkAddVersionPinned $ unlines dockerfile
            it "apk add version pinned regression - one missed" $
                let dockerfile =
                        [ "RUN apk add --no-cache \\"
                        , "flex=2.6.4-r1 \\"
                        , "libffi \\"
                        , "python2=2.7.13-r1 \\"
                        , "libbz2=1.0.6-r5"
                        ]
                in ruleCatches apkAddVersionPinned $ unlines dockerfile
            it "apk add with --no-cache" $ ruleCatches apkAddNoCache "RUN apk add flex=2.6.4-r1"
            it "apk add without --no-cache" $
                ruleCatchesNot apkAddNoCache "RUN apk add --no-cache flex=2.6.4-r1"
            it "apk add virtual package" $
                let dockerfile =
                        [ "RUN apk add \\"
                        , "--virtual build-dependencies \\"
                        , "python-dev=1.1.1 build-base=2.2.2 wget=3.3.3 \\"
                        , "&& pip install -r requirements.txt \\"
                        , "&& python setup.py install \\"
                        , "&& apk del build-dependencies"
                        ]
                in ruleCatchesNot apkAddVersionPinned $ unlines dockerfile
        --
        describe "EXPOSE rules" $ do
            it "invalid port" $ ruleCatches invalidPort "EXPOSE 80000"
            it "valid port" $ ruleCatchesNot invalidPort "EXPOSE 60000"
        --
        describe "pip pinning" $ do
            it "pip2 version not pinned" $
                ruleCatches pipVersionPinned "RUN pip2 install MySQL_python"
            it "pip3 version not pinned" $
                ruleCatches pipVersionPinned "RUN pip3 install MySQL_python"
            it "pip3 version pinned" $
                ruleCatchesNot pipVersionPinned "RUN pip3 install MySQL_python==1.2.2"
            it "pip install requirements" $
                ruleCatchesNot pipVersionPinned "RUN pip install -r requirements.txt"
            it "pip install use setup.py" $
                ruleCatchesNot pipVersionPinned "RUN pip install ."
            it "pip version not pinned" $
                ruleCatches pipVersionPinned "RUN pip install MySQL_python"
            it "pip version pinned" $
                ruleCatchesNot pipVersionPinned "RUN pip install MySQL_python==1.2.2"
            it "pip version pinned with flag" $
                ruleCatchesNot pipVersionPinned "RUN pip install --ignore-installed MySQL_python==1.2.2"
            it "pip install git" $
                ruleCatchesNot
                    pipVersionPinned
                    "RUN pip install git+https://github.com/rtfd/r-ext.git@0.6-alpha#egg=r-ext"
            it "pip install unversioned git" $
                ruleCatches
                    pipVersionPinned
                    "RUN pip install git+https://github.com/rtfd/read-ext.git#egg=read-ext"
            it "pip install upper bound" $
                ruleCatchesNot pipVersionPinned "RUN pip install 'alabaster>=0.7'"
            it "pip install lower bound" $
                ruleCatchesNot pipVersionPinned "RUN pip install 'alabaster<0.7'"
            it "pip install excluded version" $
                ruleCatchesNot pipVersionPinned "RUN pip install 'alabaster!=0.7'"
            it "pip install user directory" $
                ruleCatchesNot pipVersionPinned "RUN pip install MySQL_python==1.2.2 --user"
            it "pip install no pip version check" $
                ruleCatchesNot
                    pipVersionPinned
                    "RUN pip install MySQL_python==1.2.2 --disable-pip-version-check"
            it "pip install no cache dir" $
                ruleCatchesNot pipVersionPinned "RUN pip install MySQL_python==1.2.2 --no-cache-dir"
        --
        describe "npm pinning" $ do
            it "version pinned in package.json" $ ruleCatchesNot npmVersionPinned "RUN npm install"
            it "version pinned in package.json with arguments" $
                ruleCatchesNot npmVersionPinned "RUN npm install --progress=false"
            it "version pinned" $ ruleCatchesNot npmVersionPinned "RUN npm install express@4.1.1"
            it "version pinned with scope" $
                ruleCatchesNot npmVersionPinned "RUN npm install @myorg/privatepackage@\">=0.1.0\""
            it "version pinned multiple packages" $
                ruleCatchesNot npmVersionPinned "RUN npm install express@\"4.1.1\" sax@0.1.1"
            it "version pinned with --global" $
                ruleCatchesNot npmVersionPinned "RUN npm install --global express@\"4.1.1\""
            it "version pinned with -g" $
                ruleCatchesNot npmVersionPinned "RUN npm install -g express@\"4.1.1\""
            it "commit pinned for git+ssh" $
                ruleCatchesNot
                    npmVersionPinned
                    "RUN npm install git+ssh://git@github.com:npm/npm.git#v1.0.27"
            it "commit pinned for git+http" $
                ruleCatchesNot
                    npmVersionPinned
                    "RUN npm install git+http://isaacs@github.com/npm/npm#semver:^5.0"
            it "commit pinned for git+https" $
                ruleCatchesNot
                    npmVersionPinned
                    "RUN npm install git+https://isaacs@github.com/npm/npm.git#v1.0.27"
            it "commit pinned for git" $
                ruleCatchesNot
                    npmVersionPinned
                    "RUN npm install git://github.com/npm/npm.git#v1.0.27"
      --version range is not supported
      --it "version pinned with scope" $
      --  ruleCatchesNot npmVersionPinned "RUN npm install @myorg/privatepackage@\">=0.1.0 <0.2.0\""
            it "version not pinned" $ ruleCatches npmVersionPinned "RUN npm install express"
            it "version not pinned with scope" $
                ruleCatches npmVersionPinned "RUN npm install @myorg/privatepackage"
            it "version not pinned multiple packages" $
                ruleCatches npmVersionPinned "RUN npm install express sax@0.1.1"
            it "version not pinned with --global" $
                ruleCatches npmVersionPinned "RUN npm install --global express"
            it "commit not pinned for git+ssh" $
                ruleCatches npmVersionPinned "RUN npm install git+ssh://git@github.com:npm/npm.git"
            it "commit not pinned for git+http" $
                ruleCatches npmVersionPinned "RUN npm install git+http://isaacs@github.com/npm/npm"
            it "commit not pinned for git+https" $
                ruleCatches
                    npmVersionPinned
                    "RUN npm install git+https://isaacs@github.com/npm/npm.git"
            it "commit not pinned for git" $
                ruleCatches npmVersionPinned "RUN npm install git://github.com/npm/npm.git"
        --
        describe "use SHELL" $ do
            it "RUN ln" $ ruleCatches useShell "RUN ln -sfv /bin/bash /bin/sh"
            it "RUN ln with unrelated symlinks" $
                ruleCatchesNot useShell "RUN ln -sf /bin/true /sbin/initctl"
            it "RUN ln with multiple acceptable commands" $
                ruleCatchesNot useShell "RUN ln -s foo bar && unrelated && something_with /bin/sh"
        --
        describe "COPY rules" $ do
            it "use add" $ ruleCatches useAdd "COPY packaged-app.tar /usr/src/app"
            it "use not add" $ ruleCatchesNot useAdd "COPY package.json /usr/src/app"
        --
        describe "other rules" $ do
            it "apt-get auto yes" $ ruleCatches aptGetYes "RUN apt-get install python"
            it "apt-get yes shortflag" $ ruleCatchesNot aptGetYes "RUN apt-get install -yq python"
            it "apt-get yes quiet level 2 implies -y" $
                ruleCatchesNot aptGetYes "RUN apt-get install -qq python"
            it "apt-get yes different pos" $
                ruleCatchesNot aptGetYes "RUN apt-get install -y python"
            it "apt-get with auto yes" $ ruleCatchesNot aptGetYes "RUN apt-get -y install python"
            it "apt-get with auto expanded yes" $
                ruleCatchesNot aptGetYes "RUN apt-get --yes install python"
            it "apt-get install recommends" $
                ruleCatchesNot
                    aptGetNoRecommends
                    "RUN apt-get install --no-install-recommends python"
            it "apt-get no install recommends" $
                ruleCatches aptGetNoRecommends "RUN apt-get install python"
            it "apt-get no install recommends" $
                ruleCatches aptGetNoRecommends "RUN apt-get -y install python"
            it "apt-get version" $
                ruleCatchesNot aptGetVersionPinned "RUN apt-get install -y python=1.2.2"
            it "apt-get pinned" $
                ruleCatchesNot
                    aptGetVersionPinned
                    "RUN apt-get -y --no-install-recommends install nodejs=0.10"
            it "has maintainer" $ ruleCatches hasNoMaintainer "FROM debian\nMAINTAINER Lukas"
            it "has maintainer first" $ ruleCatches hasNoMaintainer "MAINTAINER Lukas\nFROM DEBIAN"
            it "has no maintainer" $ ruleCatchesNot hasNoMaintainer "FROM debian"
            it "using add" $ ruleCatches copyInsteadAdd "ADD file /usr/src/app/"
            it "many cmds" $ ruleCatches multipleCmds "CMD /bin/true\nCMD /bin/true"
            it "single cmd" $ ruleCatchesNot multipleCmds "CMD /bin/true"
            it "no cmd" $ ruleCatchesNot multipleEntrypoints "FROM busybox"
            it "many entries" $
                ruleCatches multipleEntrypoints "ENTRYPOINT /bin/true\nENTRYPOINT /bin/true"
            it "single entry" $ ruleCatchesNot multipleEntrypoints "ENTRYPOINT /bin/true"
            it "no entry" $ ruleCatchesNot multipleEntrypoints "FROM busybox"
            it "workdir variable" $ ruleCatchesNot absoluteWorkdir "WORKDIR ${work}"
            it "scratch" $ ruleCatchesNot noUntagged "FROM scratch"
        --
        describe "add files and archives" $ do
            it "add for tar" $ ruleCatchesNot copyInsteadAdd "ADD file.tar /usr/src/app/"
            it "add for zip" $ ruleCatchesNot copyInsteadAdd "ADD file.zip /usr/src/app/"
            it "add for gzip" $ ruleCatchesNot copyInsteadAdd "ADD file.gz /usr/src/app/"
            it "add for bz2" $ ruleCatchesNot copyInsteadAdd "ADD file.bz2 /usr/src/app/"
            it "add for xz" $ ruleCatchesNot copyInsteadAdd "ADD file.xz /usr/src/app/"
            it "add for tgz" $ ruleCatchesNot copyInsteadAdd "ADD file.tgz /usr/src/app/"
            it "add for url" $ ruleCatchesNot copyInsteadAdd "ADD http://file.com /usr/src/app/"
        --
        describe "copy last argument" $ do
            it "no warn on 2 args" $ ruleCatchesNot copyEndingSlash "COPY foo bar"
            it "warn on 3 args" $ ruleCatches copyEndingSlash "COPY foo bar baz"
            it "no warn on 3 args" $ ruleCatchesNot copyEndingSlash "COPY foo bar baz/"
        --
        describe "copy from existing alias" $ do
            it "warn on missing alias" $ ruleCatches copyFromExists "COPY --from=foo bar ."
            it "warn on alias defined after" $
                let dockerfile =
                        [ "FROM scratch"
                        , "COPY --from=build foo ."
                        , "FROM node as build"
                        , "RUN baz"
                        ]
                in ruleCatches copyFromExists $ unlines dockerfile
            it "don't warn on correctly defined aliases" $
                let dockerfile =
                        [ "FROM scratch as build"
                        , "RUN foo"
                        , "FROM node"
                        , "COPY --from=build foo ."
                        , "RUN baz"
                        ]
                in ruleCatchesNot copyFromExists $ unlines dockerfile
        --
        describe "copy from own FROM" $ do
            it "warn on copying from your the same FROM" $
                let dockerfile =
                        [ "FROM node as foo"
                        , "COPY --from=foo bar ."
                        ]
                in ruleCatches copyFromAnother $ unlines dockerfile
            it "don't warn on copying form other sources" $
                let dockerfile =
                        [ "FROM scratch as build"
                        , "RUN foo"
                        , "FROM node as run"
                        , "COPY --from=build foo ."
                        , "RUN baz"
                        ]
                in ruleCatchesNot copyFromAnother $ unlines dockerfile
        --
        describe "Duplicate aliases" $ do
            it "warn on duplicate aliases" $
                let dockerfile =
                        [ "FROM node as foo"
                        , "RUN something"
                        , "FROM scratch as foo"
                        , "RUN something"
                        ]
                in ruleCatches fromAliasUnique $ unlines dockerfile
            it "don't warn on unique aliases" $
                let dockerfile =
                        [ "FROM scratch as build"
                        , "RUN foo"
                        , "FROM node as run"
                        , "RUN baz"
                        ]
                in ruleCatchesNot fromAliasUnique $ unlines dockerfile
        --
        describe "format error" $
            it "display error after line pos" $ do
                let ast = parseString "FOM debian:jessie"
                    expectedMsg = "<string>:1:2 unexpected \"O\" expecting FROM"
                case ast of
                    Left err -> assertEqual "Unexpected error msg" expectedMsg (formatError err)
                    Right _ -> assertFailure "AST should fail parsing"

assertChecks rule s f =
    case parseString (s ++ "\n") of
        Left err -> assertFailure $ show err
        Right dockerfile -> f $ analyze [rule] dockerfile

-- Assert a failed check exists for rule
ruleCatches :: Rule -> String -> Assertion
ruleCatches rule s = assertChecks rule s f
  where
    f checks = assertEqual "No check for rule found" 1 $ length checks

ruleCatchesNot :: Rule -> String -> Assertion
ruleCatchesNot rule s = assertChecks rule s f
  where
    f checks = assertEqual "Found check of rule" 0 $ length checks
