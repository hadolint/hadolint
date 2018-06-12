{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit hiding (Label)
import Test.Hspec

import Hadolint.Formatter.TTY (formatError)
import Hadolint.Rules

import Language.Docker.Parser
import Language.Docker.Syntax
import Data.Semigroup ((<>))
import qualified Data.Text as Text

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
                    "FROM hub.docker.io/debian@sha256:\
                    \7959ed6f7e35f8b1aaa06d1d8259d4ee25aa85a086d5c125480c333183f9deeb"
            it "explicit tagged with name" $
                ruleCatchesNot noLatestTag "FROM debian:jessie AS builder"
            it "local aliases are OK to be untagged" $
                let dockerFile =
                        [ "FROM golang:1.9.3-alpine3.7 AS build"
                        , "RUN foo"
                        , "FROM build as unit-test"
                        , "RUN bar"
                        , "FROM alpine:3.7"
                        , "RUN baz"
                        ]
                in do
                  ruleCatchesNot noUntagged $ Text.unlines dockerFile
                  onBuildRuleCatchesNot noUntagged $ Text.unlines dockerFile
            it "other untagged cases are not ok" $
                let dockerFile =
                        [ "FROM golang:1.9.3-alpine3.7 AS build"
                        , "RUN foo"
                        , "FROM node as unit-test"
                        , "RUN bar"
                        , "FROM alpine:3.7"
                        , "RUN baz"
                        ]
                in do
                  ruleCatches noUntagged $ Text.unlines dockerFile
                  onBuildRuleCatches noUntagged $ Text.unlines dockerFile
        --
        describe "no root or sudo rules" $ do
            it "sudo" $ do
              ruleCatches noSudo "RUN sudo apt-get update"
              onBuildRuleCatches noSudo "RUN sudo apt-get update"
            it "no root" $ ruleCatches noRootUser "USER root"
            it "no root" $ ruleCatchesNot noRootUser "USER foo"
            it "no root UID" $ ruleCatches noRootUser "USER 0"
            it "no root:root" $ ruleCatches noRootUser "USER root:root"
            it "no root UID:GID" $ ruleCatches noRootUser "USER 0:0"
            it "install sudo" $ do
                ruleCatchesNot noSudo "RUN apt-get install sudo"
                onBuildRuleCatchesNot noSudo "RUN apt-get install sudo"
            it "sudo chained programs" $ do
                ruleCatches noSudo "RUN apt-get update && sudo apt-get install"
                onBuildRuleCatches noSudo "RUN apt-get update && sudo apt-get install"
        --
        describe "invalid CMD rules" $ do
            it "invalid cmd" $ do
                ruleCatches invalidCmd "RUN top"
                onBuildRuleCatches invalidCmd "RUN top"
            it "install ssh" $ do
                ruleCatchesNot invalidCmd "RUN apt-get install ssh"
                onBuildRuleCatchesNot invalidCmd "RUN apt-get install ssh"
        --
        describe "apt-get rules" $ do
            it "apt upgrade" $ do
                ruleCatches noAptGetUpgrade "RUN apt-get update && apt-get upgrade"
                onBuildRuleCatches noAptGetUpgrade "RUN apt-get update && apt-get upgrade"
            it "apt-get version pinning" $ do
                ruleCatches aptGetVersionPinned "RUN apt-get update && apt-get install python"
                onBuildRuleCatches aptGetVersionPinned "RUN apt-get update && apt-get install python"
            it "apt-get no cleanup" $
                let dockerFile =
                        [ "FROM scratch"
                        , "RUN apt-get update && apt-get install python"
                        ]
                in do
                  ruleCatches aptGetCleanup $ Text.unlines dockerFile
                  onBuildRuleCatches aptGetCleanup $ Text.unlines dockerFile
            it "apt-get cleanup in stage image" $
                let dockerFile =
                        [ "FROM ubuntu as foo"
                        , "RUN apt-get update && apt-get install python"
                        , "FROM scratch"
                        , "RUN echo hey!"
                        ]
                in do
                  ruleCatchesNot aptGetCleanup $ Text.unlines dockerFile
                  onBuildRuleCatchesNot aptGetCleanup $ Text.unlines dockerFile
            it "apt-get no cleanup in last stage" $
                let dockerFile =
                        [ "FROM ubuntu as foo"
                        , "RUN hey!"
                        , "FROM scratch"
                        , "RUN apt-get update && apt-get install python"
                        ]
                in do
                  ruleCatches aptGetCleanup $ Text.unlines dockerFile
                  onBuildRuleCatches aptGetCleanup $ Text.unlines dockerFile
            it "apt-get no cleanup in intermediate stage" $
                let dockerFile =
                        [ "FROM ubuntu as foo"
                        , "RUN apt-get update && apt-get install python"
                        , "FROM foo"
                        , "RUN hey!"
                        ]
                in do
                  ruleCatches aptGetCleanup $ Text.unlines dockerFile
                  onBuildRuleCatches aptGetCleanup $ Text.unlines dockerFile
            it "no warn apt-get cleanup in intermediate stage that cleans lists" $
                let dockerFile =
                        [ "FROM ubuntu as foo"
                        , "RUN apt-get update && apt-get install python && rm -rf /var/lib/apt/lists/*"
                        , "FROM foo"
                        , "RUN hey!"
                        ]
                in do
                  ruleCatchesNot aptGetCleanup $ Text.unlines dockerFile
                  onBuildRuleCatchesNot aptGetCleanup $ Text.unlines dockerFile
            it "no warn apt-get cleanup in intermediate stage when stage not used later" $
                let dockerFile =
                        [ "FROM ubuntu as foo"
                        , "RUN apt-get update && apt-get install python"
                        , "FROM scratch"
                        , "RUN hey!"
                        ]
                in do
                  ruleCatchesNot aptGetCleanup $ Text.unlines dockerFile
                  onBuildRuleCatchesNot aptGetCleanup $ Text.unlines dockerFile
            it "apt-get cleanup" $
                let dockerFile =
                        [ "FROM scratch"
                        , "RUN apt-get update && apt-get install python && rm -rf /var/lib/apt/lists/*"
                        ]
                in do
                  ruleCatchesNot aptGetCleanup $ Text.unlines dockerFile
                  onBuildRuleCatchesNot aptGetCleanup $ Text.unlines dockerFile

            it "apt-get pinned chained" $
                let dockerFile =
                        [ "RUN apt-get update \\"
                        , " && apt-get -yqq --no-install-recommends install nodejs=0.10 \\"
                        , " && rm -rf /var/lib/apt/lists/*"
                        ]
                in do
                  ruleCatchesNot aptGetVersionPinned $ Text.unlines dockerFile
                  onBuildRuleCatchesNot aptGetVersionPinned $ Text.unlines dockerFile

            it "apt-get pinned regression" $
                let dockerFile =
                        [ "RUN apt-get update && apt-get install --no-install-recommends -y \\"
                        , "python-demjson=2.2.2* \\"
                        , "wget=1.16.1* \\"
                        , "git=1:2.5.0* \\"
                        , "ruby=1:2.1.*"
                        ]
                in do
                  ruleCatchesNot aptGetVersionPinned $ Text.unlines dockerFile
                  onBuildRuleCatchesNot aptGetVersionPinned $ Text.unlines dockerFile

            it "has deprecated maintainer" $
                ruleCatches hasNoMaintainer "FROM busybox\nMAINTAINER hudu@mail.com"
        --
        describe "apk add rules" $ do
            it "apk upgrade" $ do
                ruleCatches noApkUpgrade "RUN apk update && apk upgrade"
                onBuildRuleCatches noApkUpgrade "RUN apk update && apk upgrade"
            it "apk add version pinning single" $ do
                ruleCatches apkAddVersionPinned "RUN apk add flex"
                onBuildRuleCatches apkAddVersionPinned "RUN apk add flex"
            it "apk add no version pinning single" $ do
                ruleCatchesNot apkAddVersionPinned "RUN apk add flex=2.6.4-r1"
                onBuildRuleCatchesNot apkAddVersionPinned "RUN apk add flex=2.6.4-r1"
            it "apk add version pinned chained" $
                let dockerFile =
                        [ "RUN apk add --no-cache flex=2.6.4-r1 \\"
                        , " && pip install -r requirements.txt"
                        ]
                in do
                  ruleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
                  onBuildRuleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
            it "apk add version pinned regression" $
                let dockerFile =
                        [ "RUN apk add --no-cache \\"
                        , "flex=2.6.4-r1 \\"
                        , "libffi=3.2.1-r3 \\"
                        , "python2=2.7.13-r1 \\"
                        , "libbz2=1.0.6-r5"
                        ]
                in do
                  ruleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
                  onBuildRuleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
            it "apk add version pinned regression - one missed" $
                let dockerFile =
                        [ "RUN apk add --no-cache \\"
                        , "flex=2.6.4-r1 \\"
                        , "libffi \\"
                        , "python2=2.7.13-r1 \\"
                        , "libbz2=1.0.6-r5"
                        ]
                in do
                  ruleCatches apkAddVersionPinned $ Text.unlines dockerFile
                  onBuildRuleCatches apkAddVersionPinned $ Text.unlines dockerFile
            it "apk add with --no-cache" $ do
                ruleCatches apkAddNoCache "RUN apk add flex=2.6.4-r1"
                onBuildRuleCatches apkAddNoCache "RUN apk add flex=2.6.4-r1"
            it "apk add without --no-cache" $ do
                ruleCatchesNot apkAddNoCache "RUN apk add --no-cache flex=2.6.4-r1"
                onBuildRuleCatchesNot apkAddNoCache "RUN apk add --no-cache flex=2.6.4-r1"
            it "apk add virtual package" $
                let dockerFile =
                        [ "RUN apk add \\"
                        , "--virtual build-dependencies \\"
                        , "python-dev=1.1.1 build-base=2.2.2 wget=3.3.3 \\"
                        , "&& pip install -r requirements.txt \\"
                        , "&& python setup.py install \\"
                        , "&& apk del build-dependencies"
                        ]
                in do
                  ruleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
                  onBuildRuleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
        --
        describe "EXPOSE rules" $ do
            it "invalid port" $ ruleCatches invalidPort "EXPOSE 80000"
            it "valid port" $ ruleCatchesNot invalidPort "EXPOSE 60000"
        --
        describe "pip pinning" $ do
            it "pip2 version not pinned" $ do
                ruleCatches pipVersionPinned "RUN pip2 install MySQL_python"
                onBuildRuleCatches pipVersionPinned "RUN pip2 install MySQL_python"
            it "pip3 version not pinned" $ do
                ruleCatches pipVersionPinned "RUN pip3 install MySQL_python"
                onBuildRuleCatches pipVersionPinned "RUN pip2 install MySQL_python"
            it "pip3 version pinned" $ do
                ruleCatchesNot pipVersionPinned "RUN pip3 install MySQL_python==1.2.2"
                onBuildRuleCatchesNot pipVersionPinned "RUN pip3 install MySQL_python==1.2.2"
            it "pip install requirements" $ do
                ruleCatchesNot pipVersionPinned "RUN pip install -r requirements.txt"
                onBuildRuleCatchesNot pipVersionPinned "RUN pip install -r requirements.txt"
            it "pip install use setup.py" $ do
                ruleCatchesNot pipVersionPinned "RUN pip install ."
                onBuildRuleCatchesNot pipVersionPinned "RUN pip install ."
            it "pip version not pinned" $ do
                ruleCatches pipVersionPinned "RUN pip install MySQL_python"
                onBuildRuleCatches pipVersionPinned "RUN pip install MySQL_python"
            it "pip version pinned" $ do
                ruleCatchesNot pipVersionPinned "RUN pip install MySQL_python==1.2.2"
                onBuildRuleCatchesNot pipVersionPinned "RUN pip install MySQL_python==1.2.2"
            it "pip version pinned with ~= operator" $ do
                ruleCatchesNot pipVersionPinned "RUN pip install MySQL_python~=1.2.2"
                onBuildRuleCatchesNot pipVersionPinned "RUN pip install MySQL_python~=1.2.2"
            it "pip version pinned with === operator" $ do
                ruleCatchesNot pipVersionPinned "RUN pip install MySQL_python===1.2.2"
                onBuildRuleCatchesNot pipVersionPinned "RUN pip install MySQL_python===1.2.2"
            it "pip version pinned with flag" $ do
                ruleCatchesNot pipVersionPinned "RUN pip install --ignore-installed MySQL_python==1.2.2"
                onBuildRuleCatchesNot pipVersionPinned "RUN pip install --ignore-installed MySQL_python==1.2.2"
            it "pip version pinned with python -m" $ do
                ruleCatchesNot pipVersionPinned "RUN python -m pip install example==1.2.2"
                onBuildRuleCatchesNot pipVersionPinned "RUN python -m pip install example==1.2.2"
            it "pip install git" $ do
                ruleCatchesNot
                    pipVersionPinned
                    "RUN pip install git+https://github.com/rtfd/r-ext.git@0.6-alpha#egg=r-ext"
                onBuildRuleCatchesNot
                    pipVersionPinned
                    "RUN pip install git+https://github.com/rtfd/r-ext.git@0.6-alpha#egg=r-ext"
            it "pip install unversioned git" $ do
                ruleCatches
                    pipVersionPinned
                    "RUN pip install git+https://github.com/rtfd/read-ext.git#egg=read-ext"
                onBuildRuleCatches
                    pipVersionPinned
                    "RUN pip install git+https://github.com/rtfd/read-ext.git#egg=read-ext"
            it "pip install upper bound" $ do
                ruleCatchesNot pipVersionPinned "RUN pip install 'alabaster>=0.7'"
                onBuildRuleCatchesNot pipVersionPinned "RUN pip install 'alabaster>=0.7'"
            it "pip install lower bound" $ do
                ruleCatchesNot pipVersionPinned "RUN pip install 'alabaster<0.7'"
                onBuildRuleCatchesNot pipVersionPinned "RUN pip install 'alabaster<0.7'"
            it "pip install excluded version" $ do
                ruleCatchesNot pipVersionPinned "RUN pip install 'alabaster!=0.7'"
                onBuildRuleCatchesNot pipVersionPinned "RUN pip install 'alabaster!=0.7'"
            it "pip install user directory" $ do
                ruleCatchesNot pipVersionPinned "RUN pip install MySQL_python==1.2.2 --user"
                onBuildRuleCatchesNot pipVersionPinned "RUN pip install MySQL_python==1.2.2 --user"
            it "pip install no pip version check" $ do
                ruleCatchesNot
                    pipVersionPinned
                    "RUN pip install MySQL_python==1.2.2 --disable-pip-version-check"
                onBuildRuleCatchesNot
                    pipVersionPinned
                    "RUN pip install MySQL_python==1.2.2 --disable-pip-version-check"
            it "pip install no cache dir" $ do
                ruleCatchesNot pipVersionPinned "RUN pip install MySQL_python==1.2.2 --no-cache-dir"
                onBuildRuleCatchesNot pipVersionPinned "RUN pip install MySQL_python==1.2.2 --no-cache-dir"
        --
        describe "npm pinning" $ do
            it "version pinned in package.json" $ do
                ruleCatchesNot npmVersionPinned "RUN npm install"
                onBuildRuleCatchesNot npmVersionPinned "RUN npm install"
            it "version pinned in package.json with arguments" $ do
                ruleCatchesNot npmVersionPinned "RUN npm install --progress=false"
                onBuildRuleCatchesNot npmVersionPinned "RUN npm install --progress=false"
            it "version pinned" $ do
                ruleCatchesNot npmVersionPinned "RUN npm install express@4.1.1"
                onBuildRuleCatchesNot npmVersionPinned "RUN npm install express@4.1.1"
            it "version pinned with scope" $ do
                ruleCatchesNot npmVersionPinned "RUN npm install @myorg/privatepackage@\">=0.1.0\""
                onBuildRuleCatchesNot npmVersionPinned "RUN npm install @myorg/privatepackage@\">=0.1.0\""
            it "version pinned multiple packages" $ do
                ruleCatchesNot npmVersionPinned "RUN npm install express@\"4.1.1\" sax@0.1.1"
                onBuildRuleCatchesNot npmVersionPinned "RUN npm install express@\"4.1.1\" sax@0.1.1"
            it "version pinned with --global" $ do
                ruleCatchesNot npmVersionPinned "RUN npm install --global express@\"4.1.1\""
                onBuildRuleCatchesNot npmVersionPinned "RUN npm install --global express@\"4.1.1\""
            it "version pinned with -g" $ do
                ruleCatchesNot npmVersionPinned "RUN npm install -g express@\"4.1.1\""
                onBuildRuleCatchesNot npmVersionPinned "RUN npm install -g express@\"4.1.1\""
            it "commit pinned for git+ssh" $ do
                ruleCatchesNot
                    npmVersionPinned
                    "RUN npm install git+ssh://git@github.com:npm/npm.git#v1.0.27"
                onBuildRuleCatchesNot
                    npmVersionPinned
                    "RUN npm install git+ssh://git@github.com:npm/npm.git#v1.0.27"
            it "commit pinned for git+http" $ do
                ruleCatchesNot
                    npmVersionPinned
                    "RUN npm install git+http://isaacs@github.com/npm/npm#semver:^5.0"
                onBuildRuleCatchesNot
                    npmVersionPinned
                    "RUN npm install git+http://isaacs@github.com/npm/npm#semver:^5.0"
            it "commit pinned for git+https" $ do
                ruleCatchesNot
                    npmVersionPinned
                    "RUN npm install git+https://isaacs@github.com/npm/npm.git#v1.0.27"
                onBuildRuleCatchesNot
                    npmVersionPinned
                    "RUN npm install git+https://isaacs@github.com/npm/npm.git#v1.0.27"
            it "commit pinned for git" $ do
                ruleCatchesNot
                    npmVersionPinned
                    "RUN npm install git://github.com/npm/npm.git#v1.0.27"
                onBuildRuleCatchesNot
                    npmVersionPinned
                    "RUN npm install git://github.com/npm/npm.git#v1.0.27"

      --version range is not supported
            it "version pinned with scope" $ do
                ruleCatchesNot npmVersionPinned "RUN npm install @myorg/privatepackage@\">=0.1.0 <0.2.0\""
                onBuildRuleCatchesNot npmVersionPinned "RUN npm install @myorg/privatepackage@\">=0.1.0 <0.2.0\""
            it "version not pinned" $ do
                ruleCatches npmVersionPinned "RUN npm install express"
                onBuildRuleCatches npmVersionPinned "RUN npm install express"
            it "version not pinned with scope" $ do
                ruleCatches npmVersionPinned "RUN npm install @myorg/privatepackage"
                onBuildRuleCatches npmVersionPinned "RUN npm install @myorg/privatepackage"
            it "version not pinned multiple packages" $ do
                ruleCatches npmVersionPinned "RUN npm install express sax@0.1.1"
                onBuildRuleCatches npmVersionPinned "RUN npm install express sax@0.1.1"
            it "version not pinned with --global" $ do
                ruleCatches npmVersionPinned "RUN npm install --global express"
                onBuildRuleCatches npmVersionPinned "RUN npm install --global express"
            it "commit not pinned for git+ssh" $ do
                ruleCatches npmVersionPinned "RUN npm install git+ssh://git@github.com:npm/npm.git"
                onBuildRuleCatches npmVersionPinned "RUN npm install git+ssh://git@github.com:npm/npm.git"
            it "commit not pinned for git+http" $ do
                ruleCatches npmVersionPinned "RUN npm install git+http://isaacs@github.com/npm/npm"
                onBuildRuleCatches npmVersionPinned "RUN npm install git+http://isaacs@github.com/npm/npm"
            it "commit not pinned for git+https" $ do
                ruleCatches
                    npmVersionPinned
                    "RUN npm install git+https://isaacs@github.com/npm/npm.git"
                onBuildRuleCatches
                    npmVersionPinned
                    "RUN npm install git+https://isaacs@github.com/npm/npm.git"
            it "commit not pinned for git" $ do
                ruleCatches npmVersionPinned "RUN npm install git://github.com/npm/npm.git"
                onBuildRuleCatches npmVersionPinned "RUN npm install git://github.com/npm/npm.git"
        --
        describe "use SHELL" $ do
            it "RUN ln" $ do
                ruleCatches useShell "RUN ln -sfv /bin/bash /bin/sh"
                onBuildRuleCatches useShell "RUN ln -sfv /bin/bash /bin/sh"
            it "RUN ln with unrelated symlinks" $ do
                ruleCatchesNot useShell "RUN ln -sf /bin/true /sbin/initctl"
                onBuildRuleCatchesNot useShell "RUN ln -sf /bin/true /sbin/initctl"
            it "RUN ln with multiple acceptable commands" $ do
                ruleCatchesNot useShell "RUN ln -s foo bar && unrelated && something_with /bin/sh"
                onBuildRuleCatchesNot useShell "RUN ln -s foo bar && unrelated && something_with /bin/sh"
        --
        describe "COPY rules" $ do
            it "use add" $ ruleCatches useAdd "COPY packaged-app.tar /usr/src/app"
            it "use not add" $ ruleCatchesNot useAdd "COPY package.json /usr/src/app"
        --
        describe "other rules" $ do
            it "apt-get auto yes" $ do
              ruleCatches aptGetYes "RUN apt-get install python"
              onBuildRuleCatches aptGetYes "RUN apt-get install python"
            it "apt-get yes shortflag" $ do
              ruleCatchesNot aptGetYes "RUN apt-get install -yq python"
              onBuildRuleCatchesNot aptGetYes "RUN apt-get install -yq python"
            it "apt-get yes quiet level 2 implies -y" $ do
                ruleCatchesNot aptGetYes "RUN apt-get install -qq python"
                onBuildRuleCatchesNot aptGetYes "RUN apt-get install -qq python"
            it "apt-get yes different pos" $ do
                ruleCatchesNot aptGetYes "RUN apt-get install -y python"
                onBuildRuleCatchesNot aptGetYes "RUN apt-get install -y python"
            it "apt-get with auto yes" $ do
                ruleCatchesNot aptGetYes "RUN apt-get -y install python"
                onBuildRuleCatchesNot aptGetYes "RUN apt-get -y install python"
            it "apt-get with auto expanded yes" $ do
                ruleCatchesNot aptGetYes "RUN apt-get --yes install python"
                onBuildRuleCatchesNot aptGetYes "RUN apt-get --yes install python"
            it "apt-get install recommends" $ do
                ruleCatchesNot
                    aptGetNoRecommends
                    "RUN apt-get install --no-install-recommends python"
                onBuildRuleCatchesNot
                    aptGetNoRecommends
                    "RUN apt-get install --no-install-recommends python"
            it "apt-get no install recommends" $ do
                ruleCatches aptGetNoRecommends "RUN apt-get install python"
                onBuildRuleCatches aptGetNoRecommends "RUN apt-get install python"
            it "apt-get no install recommends" $ do
                ruleCatches aptGetNoRecommends "RUN apt-get -y install python"
                onBuildRuleCatches aptGetNoRecommends "RUN apt-get -y install python"
            it "apt-get version" $ do
                ruleCatchesNot aptGetVersionPinned "RUN apt-get install -y python=1.2.2"
                onBuildRuleCatchesNot aptGetVersionPinned "RUN apt-get install -y python=1.2.2"
            it "apt-get pinned" $ do
                ruleCatchesNot
                    aptGetVersionPinned
                    "RUN apt-get -y --no-install-recommends install nodejs=0.10"
                onBuildRuleCatchesNot
                    aptGetVersionPinned
                    "RUN apt-get -y --no-install-recommends install nodejs=0.10"
            it "apt-get tolerate target-release" $
                let dockerFile =
                        [ "RUN set -e &&\\"
                        , " echo \"deb http://http.debian.net/debian jessie-backports main\" \
                          \> /etc/apt/sources.list.d/jessie-backports.list &&\\"
                        , " apt-get update &&\\"
                        , " apt-get install -y --no-install-recommends -t jessie-backports \
                          \openjdk-8-jdk=8u131-b11-1~bpo8+1 &&\\"
                        , " rm -rf /var/lib/apt/lists/*"
                        ]
                in do
                  ruleCatchesNot aptGetVersionPinned $ Text.unlines dockerFile
                  onBuildRuleCatchesNot aptGetVersionPinned $ Text.unlines dockerFile

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
                let dockerFile =
                        [ "FROM scratch"
                        , "COPY --from=build foo ."
                        , "FROM node as build"
                        , "RUN baz"
                        ]
                in ruleCatches copyFromExists $ Text.unlines dockerFile
            it "don't warn on correctly defined aliases" $
                let dockerFile =
                        [ "FROM scratch as build"
                        , "RUN foo"
                        , "FROM node"
                        , "COPY --from=build foo ."
                        , "RUN baz"
                        ]
                in ruleCatchesNot copyFromExists $ Text.unlines dockerFile
        --
        describe "copy from own FROM" $ do
            it "warn on copying from your the same FROM" $
                let dockerFile =
                        [ "FROM node as foo"
                        , "COPY --from=foo bar ."
                        ]
                in ruleCatches copyFromAnother $ Text.unlines dockerFile
            it "don't warn on copying form other sources" $
                let dockerFile =
                        [ "FROM scratch as build"
                        , "RUN foo"
                        , "FROM node as run"
                        , "COPY --from=build foo ."
                        , "RUN baz"
                        ]
                in ruleCatchesNot copyFromAnother $ Text.unlines dockerFile
        --
        describe "Duplicate aliases" $ do
            it "warn on duplicate aliases" $
                let dockerFile =
                        [ "FROM node as foo"
                        , "RUN something"
                        , "FROM scratch as foo"
                        , "RUN something"
                        ]
                in ruleCatches fromAliasUnique $ Text.unlines dockerFile
            it "don't warn on unique aliases" $
                let dockerFile =
                        [ "FROM scratch as build"
                        , "RUN foo"
                        , "FROM node as run"
                        , "RUN baz"
                        ]
                in ruleCatchesNot fromAliasUnique $ Text.unlines dockerFile
        --
        describe "format error" $
            it "display error after line pos" $ do
                let ast = parseText "FOM debian:jessie"
                    expectedMsg = "<string>:1:1 unexpected 'F' expecting '#', ADD, ARG, CMD, COPY, ENTRYPOINT, " <>
                                  "ENV, EXPOSE, FROM, HEALTHCHECK, LABEL, MAINTAINER, ONBUILD, RUN, SHELL, STOPSIGNAL, " <>
                                  "USER, VOLUME, WORKDIR, or end of input "
                case ast of
                    Left err -> assertEqual "Unexpected error msg" expectedMsg (formatError err)
                    Right _ -> assertFailure "AST should fail parsing"
        --
        describe "Rules can be ignored with inline comments" $ do
            it "ignores single rule" $
                let dockerFile =
                        [ "FROM ubuntu"
                        , "# hadolint ignore=DL3002"
                        , "USER root"
                        ]
                in ruleCatchesNot noRootUser $ Text.unlines dockerFile
            it "ignores only the given rule" $
                let dockerFile =
                        [ "# hadolint ignore=DL3001"
                        , "USER root"
                        ]
                in ruleCatches noRootUser $ Text.unlines dockerFile
            it "ignores only the given rule, when multiple passed" $
                let dockerFile =
                        [ "# hadolint ignore=DL3001,DL3002"
                        , "USER root"
                        ]
                in ruleCatchesNot noRootUser $ Text.unlines dockerFile
            it "ignores the rule only if directly above the instruction" $
                let dockerFile =
                        [ "# hadolint ignore=DL3001,DL3002"
                        , "FROM ubuntu"
                        , "USER root"
                        ]
                in ruleCatches noRootUser $ Text.unlines dockerFile
            it "won't ignore the rule if passed invalid rule names" $
                let dockerFile =
                        [ "# hadolint ignore=crazy,DL3002"
                        , "USER root"
                        ]
                in ruleCatches noRootUser $ Text.unlines dockerFile
            it "ignores multiple rules correctly, even with some extra whitespace" $
                let dockerFile =
                        [ "FROM node as foo"
                        , "# hadolint ignore=DL3023, DL3021"
                        , "COPY --from=foo bar baz ."
                        ]
                in do
                  ruleCatchesNot copyFromAnother $ Text.unlines dockerFile
                  ruleCatchesNot copyEndingSlash $ Text.unlines dockerFile

assertChecks :: HasCallStack => Rule -> Text.Text -> ([RuleCheck] -> IO a) -> IO a
assertChecks rule s makeAssertions =
    case parseText (s <> "\n") of
        Left err -> assertFailure $ show err
        Right dockerFile -> makeAssertions $ analyze [rule] dockerFile


assertOnBuildChecks :: HasCallStack => Rule -> Text.Text -> ([RuleCheck] -> IO a) -> IO a
assertOnBuildChecks rule s makeAssertions =
    case parseText (s <> "\n") of
        Left err -> assertFailure $ show err
        Right dockerFile -> checkOnBuild dockerFile
  where
    checkOnBuild dockerFile = makeAssertions $ analyze [rule] (fmap wrapInOnBuild dockerFile)
    wrapInOnBuild (InstructionPos (Run args) so li) = InstructionPos (OnBuild (Run args)) so li
    wrapInOnBuild i = i

-- Assert a failed check exists for rule
ruleCatches :: HasCallStack => Rule -> Text.Text -> Assertion
ruleCatches rule s = assertChecks rule s f
  where
    f checks = do
      assertEqual "No check for rule found" 1 $ length checks
      assertBool "Incorrect line number for result" $ null [c | c <- checks, linenumber c <= 0]

onBuildRuleCatches :: HasCallStack => Rule -> Text.Text -> Assertion
onBuildRuleCatches rule s = assertOnBuildChecks rule s f
  where
    f checks = do
      assertEqual "No check for rule found" 1 $ length checks
      assertBool "Incorrect line number for result" $ null [c | c <- checks, linenumber c <= 0]

ruleCatchesNot :: HasCallStack => Rule -> Text.Text -> Assertion
ruleCatchesNot rule s = assertChecks rule s f
  where
    f checks = assertEqual "Found check of rule" 0 $ length checks

onBuildRuleCatchesNot :: HasCallStack => Rule -> Text.Text -> Assertion
onBuildRuleCatchesNot rule s = assertOnBuildChecks rule s f
  where
    f checks = assertEqual "Found check of rule" 0 $ length checks
