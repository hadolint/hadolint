{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified ConfigSpec
import Control.Monad (unless, when)
import qualified Data.Text as Text
import Hadolint.Formatter.TTY (formatChecks, formatError)
import Hadolint.Rules
import Language.Docker.Parser
import Language.Docker.Syntax
import Test.HUnit hiding (Label)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "FROM rules" $ do
      it "no untagged" $ ruleCatches noUntagged "FROM debian"
      it "no untagged with name" $ ruleCatches noUntagged "FROM debian AS builder"
      it "explicit latest" $ ruleCatches noLatestTag "FROM debian:latest"
      it "explicit latest with name" $ ruleCatches noLatestTag "FROM debian:latest AS builder"
      it "explicit tagged" $ ruleCatchesNot noLatestTag "FROM debian:jessie"
      it "explicit platform flag" $ ruleCatches noPlatformFlag "FROM --platform=linux debian:jessie"
      it "no platform flag" $ ruleCatchesNot noPlatformFlag "FROM debian:jessie"
      it "explicit SHA" $
        ruleCatchesNot
          noLatestTag
          "FROM hub.docker.io/debian@sha256:\
          \7959ed6f7e35f8b1aaa06d1d8259d4ee25aa85a086d5c125480c333183f9deeb"
      it "explicit tagged with name" $
        ruleCatchesNot noLatestTag "FROM debian:jessie AS builder"
      it "untagged digest is not an error" $
        ruleCatchesNot noUntagged "FROM ruby@sha256:f1dbca0f5dbc9"
      it "untagged digest is not an error" $
        ruleCatchesNot noUntagged "FROM ruby:2"
      it "local aliases are OK to be untagged" $
        let dockerFile =
              [ "FROM golang:1.9.3-alpine3.7 AS build",
                "RUN foo",
                "FROM build as unit-test",
                "RUN bar",
                "FROM alpine:3.7",
                "RUN baz"
              ]
         in do
              ruleCatchesNot noUntagged $ Text.unlines dockerFile
              onBuildRuleCatchesNot noUntagged $ Text.unlines dockerFile
      it "other untagged cases are not ok" $
        let dockerFile =
              [ "FROM golang:1.9.3-alpine3.7 AS build",
                "RUN foo",
                "FROM node as unit-test",
                "RUN bar",
                "FROM alpine:3.7",
                "RUN baz"
              ]
         in do
              ruleCatches noUntagged $ Text.unlines dockerFile
              onBuildRuleCatches noUntagged $ Text.unlines dockerFile
    --
    describe "no root or sudo rules" $ do
      it "sudo" $ do
        ruleCatches noSudo "RUN sudo apt-get update"
        onBuildRuleCatches noSudo "RUN sudo apt-get update"

      it "last user should not be root" $
        let dockerFile =
              [ "FROM scratch",
                "USER root"
              ]
         in ruleCatches noRootUser $ Text.unlines dockerFile

      it "no root" $
        let dockerFile =
              [ "FROM scratch",
                "USER foo"
              ]
         in ruleCatchesNot noRootUser $ Text.unlines dockerFile

      it "no root UID" $
        let dockerFile =
              [ "FROM scratch",
                "USER 0"
              ]
         in ruleCatches noRootUser $ Text.unlines dockerFile

      it "no root:root" $
        let dockerFile =
              [ "FROM scratch",
                "USER root:root"
              ]
         in ruleCatches noRootUser $ Text.unlines dockerFile

      it "no UID:GID" $
        let dockerFile =
              [ "FROM scratch",
                "USER 0:0"
              ]
         in ruleCatches noRootUser $ Text.unlines dockerFile

      it "can switch back to non root" $
        let dockerFile =
              [ "FROM scratch",
                "USER root",
                "RUN something",
                "USER foo"
              ]
         in ruleCatchesNot noRootUser $ Text.unlines dockerFile

      it "warns on transitive root user" $
        let dockerFile =
              [ "FROM debian as base",
                "USER root",
                "RUN something",
                "FROM base",
                "RUN something else"
              ]
         in ruleCatches noRootUser $ Text.unlines dockerFile

      it "warns on multiple stages" $
        let dockerFile =
              [ "FROM debian as base",
                "USER root",
                "RUN something",
                "FROM scratch",
                "USER foo",
                "RUN something else"
              ]
         in ruleCatches noRootUser $ Text.unlines dockerFile

      it "does not warn when switching in multiple stages" $
        let dockerFile =
              [ "FROM debian as base",
                "USER root",
                "RUN something",
                "USER foo",
                "FROM scratch",
                "RUN something else"
              ]
         in ruleCatchesNot noRootUser $ Text.unlines dockerFile

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
    describe "gem" $
      describe "version pinning" $ do
        describe "i" $ do
          it "unpinned" $ do
            ruleCatches gemVersionPinned "RUN gem i bundler"
            onBuildRuleCatches gemVersionPinned "RUN gem i bundler"
          it "pinned" $ do
            ruleCatchesNot gemVersionPinned "RUN gem i bundler:1"
            onBuildRuleCatchesNot gemVersionPinned "RUN gem i bundler:1"
          it "multi" $ do
            ruleCatches gemVersionPinned "RUN gem i bunlder:1 nokogiri"
            onBuildRuleCatches gemVersionPinned "RUN gem i bunlder:1 nokogiri"
            ruleCatchesNot gemVersionPinned "RUN gem i bunlder:1 nokogirii:1"
            onBuildRuleCatchesNot gemVersionPinned "RUN gem i bunlder:1 nokogiri:1"
        describe "install" $ do
          it "unpinned" $ do
            ruleCatches gemVersionPinned "RUN gem install bundler"
            onBuildRuleCatches gemVersionPinned "RUN gem install bundler"
          it "pinned" $ do
            ruleCatchesNot gemVersionPinned "RUN gem install bundler:1"
            onBuildRuleCatchesNot gemVersionPinned "RUN gem install bundler:1"
          it "does not warn on -v" $ do
            ruleCatchesNot gemVersionPinned "RUN gem install bundler -v '2.0.1'"
            onBuildRuleCatchesNot gemVersionPinned "RUN gem install bundler -v '2.0.1'"
          it "does not warn on --version without =" $ do
            ruleCatchesNot gemVersionPinned "RUN gem install bundler --version '2.0.1'"
            onBuildRuleCatchesNot gemVersionPinned "RUN gem install bundler --version '2.0.1'"
          it "does not warn on --version with =" $ do
            ruleCatchesNot gemVersionPinned "RUN gem install bundler --version='2.0.1'"
            onBuildRuleCatchesNot gemVersionPinned "RUN gem install bundler --version='2.0.1'"
          it "does not warn on extra flags" $ do
            ruleCatchesNot gemVersionPinned "RUN gem install bundler:2.0.1 -- --use-system-libraries=true"
            onBuildRuleCatchesNot gemVersionPinned "RUN gem install bundler:2.0.1 -- --use-system-libraries=true"
    --
    describe "yum rules" $ do
        it "yum update" $ do
            ruleCatches noYumUpdate "RUN yum update"
            ruleCatchesNot noYumUpdate "RUN yum install -y httpd-2.4.42 && yum clean all"
            ruleCatchesNot noYumUpdate "RUN bash -c `# not even a yum command`"
            onBuildRuleCatches noYumUpdate "RUN yum update"
            onBuildRuleCatchesNot noYumUpdate "RUN yum install -y httpd-2.4.42 && yum clean all"
            onBuildRuleCatchesNot noYumUpdate "RUN bash -c `# not even a yum command`"
        it "yum version pinning" $ do
            ruleCatches yumVersionPinned "RUN yum install -y tomcat && yum clean all"
            ruleCatchesNot yumVersionPinned "RUN yum install -y tomcat-9.2 && yum clean all"
            ruleCatchesNot yumVersionPinned "RUN bash -c `# not even a yum command`"
            onBuildRuleCatches yumVersionPinned "RUN yum install -y tomcat && yum clean all"
            onBuildRuleCatchesNot yumVersionPinned "RUN yum install -y tomcat-9.2 && yum clean all"
            onBuildRuleCatchesNot yumVersionPinned "RUN bash -c `# not even a yum command`"
        it "yum no clean all" $ do
            ruleCatches yumCleanup "RUN yum install -y mariadb-10.4"
            ruleCatchesNot yumCleanup "RUN yum install -y mariadb-10.4 && yum clean all"
            ruleCatchesNot yumCleanup "RUN bash -c `# not even a yum command`"
            onBuildRuleCatches yumCleanup "RUN yum install -y mariadb-10.4"
            onBuildRuleCatchesNot yumCleanup "RUN yum install -y mariadb-10.4 && yum clean all"
            onBuildRuleCatchesNot yumCleanup "RUN bash -c `# not even a yum command`"
        it "yum non-interactive" $ do
            ruleCatches yumYes "RUN yum install httpd-2.4.24 && yum clean all"
            ruleCatchesNot yumYes "RUN yum install -y httpd-2.4.24 && yum clean all"
            ruleCatchesNot yumYes "RUN bash -c `# not even a yum command`"
            onBuildRuleCatches yumYes "RUN yum install httpd-2.4.24 && yum clean all"
            onBuildRuleCatchesNot yumYes "RUN yum install -y httpd-2.4.24 && yum clean all"
            onBuildRuleCatchesNot yumYes "RUN bash -c `# not even a yum command`"
    --
    describe "zypper rules" $ do
        it "zupper update" $ do
            ruleCatches noZypperUpdate "RUN zypper update"
            ruleCatches noZypperUpdate "RUN zypper up"
            ruleCatches noZypperUpdate "RUN zypper dist-upgrade"
            ruleCatches noZypperUpdate "RUN zypper dup"
            onBuildRuleCatches noZypperUpdate "RUN zypper update"
            onBuildRuleCatches noZypperUpdate "RUN zypper up"
            onBuildRuleCatches noZypperUpdate "RUN zypper dist-upgrade"
            onBuildRuleCatches noZypperUpdate "RUN zypper dup"
        it "zypper version pinning" $ do
-- NOTE: In Haskell strings, '\' has to be escaped. And in shell commands, '>'
-- and '<' have to be escaped. Hence the double escaping.
            ruleCatches zypperVersionPinned "RUN zypper install -y tomcat && zypper clean"
            ruleCatchesNot zypperVersionPinned "RUN zypper install -y tomcat=9.0.39 && zypper clean"
            ruleCatchesNot zypperVersionPinned "RUN zypper install -y tomcat\\>=9.0 && zypper clean"
            ruleCatchesNot zypperVersionPinned "RUN zypper install -y tomcat\\>9.0 && zypper clean"
            ruleCatchesNot zypperVersionPinned "RUN zypper install -y tomcat\\<=9.0 && zypper clean"
            ruleCatchesNot zypperVersionPinned "RUN zypper install -y tomcat\\<9.0 && zypper clean"
            ruleCatchesNot zypperVersionPinned "RUN zypper install -y tomcat-9.0.39-1.rpm && zypper clean"
            onBuildRuleCatches zypperVersionPinned "RUN zypper install -y tomcat && zypper clean"
            onBuildRuleCatchesNot zypperVersionPinned "RUN zypper install -y tomcat=9.0.39 && zypper clean"
            onBuildRuleCatchesNot zypperVersionPinned "RUN zypper install -y tomcat\\>=9.0 && zypper clean"
            onBuildRuleCatchesNot zypperVersionPinned "RUN zypper install -y tomcat\\>9.0 && zypper clean"
            onBuildRuleCatchesNot zypperVersionPinned "RUN zypper install -y tomcat\\<=9.0 && zypper clean"
            onBuildRuleCatchesNot zypperVersionPinned "RUN zypper install -y tomcat\\<9.0 && zypper clean"
            onBuildRuleCatchesNot zypperVersionPinned "RUN zypper install -y tomcat-9.0.39-1.rpm && zypper clean"
        it "zypper no clean all" $ do
            ruleCatches zypperCleanup "RUN zypper install -y mariadb=10.4"
            ruleCatchesNot zypperCleanup "RUN zypper install -y mariadb=10.4 && zypper clean"
            ruleCatchesNot zypperCleanup "RUN zypper install -y mariadb=10.4 && zypper cc"
            onBuildRuleCatches zypperCleanup "RUN zypper install -y mariadb=10.4"
            onBuildRuleCatchesNot zypperCleanup "RUN zypper install -y mariadb=10.4 && zypper clean"
            onBuildRuleCatchesNot zypperCleanup "RUN zypper install -y mariadb=10.4 && zypper cc"
        it "zypper non-interactive" $ do
            ruleCatches zypperYes "RUN zypper install httpd=2.4.24 && zypper clean"
            ruleCatchesNot zypperYes "RUN zypper install -y httpd=2.4.24 && zypper clean"
            ruleCatchesNot zypperYes "RUN zypper install --no-confirm httpd=2.4.24 && zypper clean"
            onBuildRuleCatches zypperYes "RUN zypper install httpd=2.4.24 && zypper clean"
            onBuildRuleCatchesNot zypperYes "RUN zypper install -y httpd=2.4.24 && zypper clean"
            onBuildRuleCatchesNot zypperYes "RUN zypper install --no-confirm httpd=2.4.24 && zypper clean"
    --
    describe "dnf rules" $ do
      it "dnf update" $ do
        ruleCatches noDnfUpdate "RUN dnf upgrade"
        onBuildRuleCatches noDnfUpdate "RUN dnf upgrade"
      it "dnf version pinning" $ do
        ruleCatches dnfVersionPinned "RUN dnf install -y tomcat && dnf clean all"
        ruleCatchesNot dnfVersionPinned "RUN dnf install -y tomcat-9.0.1 && dnf clean all"
        onBuildRuleCatches dnfVersionPinned "RUN dnf install -y tomcat && dnf clean all"
        onBuildRuleCatchesNot dnfVersionPinned "RUN dnf install -y tomcat-9.0.1 && dnf clean all"
      it "dnf no clean all" $ do
        ruleCatches dnfCleanup "RUN dnf install -y mariadb-10.4"
        ruleCatchesNot dnfCleanup "RUN dnf install -y mariadb-10.4 && dnf clean all"
        onBuildRuleCatches dnfCleanup "RUN dnf install -y mariadb-10.4"
        onBuildRuleCatchesNot dnfCleanup "RUN dnf install -y mariadb-10.4 && dnf clean all"
      it "dnf non-interactive" $ do
        ruleCatches dnfYes "RUN dnf install httpd-2.4.24 && dnf clean all"
        ruleCatchesNot dnfYes "RUN dnf install -y httpd-2.4.24 && dnf clean all"
        onBuildRuleCatches dnfYes "RUN dnf install httpd-2.4.24 && dnf clean all"
        onBuildRuleCatchesNot dnfYes "RUN dnf install -y httpd-2.4.24 && dnf clean all"
    --
    describe "dnf rules" $ do
      it "dnf update" $ do
        ruleCatches noDnfUpdate "RUN dnf upgrade"
        ruleCatches noDnfUpdate "RUN dnf upgrade-minimal"
        ruleCatchesNot noDnfUpdate "RUN notdnf upgrade"
        onBuildRuleCatches noDnfUpdate "RUN dnf upgrade"
        onBuildRuleCatches noDnfUpdate "RUN dnf upgrade-minimal"
        onBuildRuleCatchesNot noDnfUpdate "RUN notdnf upgrade"
      it "dnf version pinning" $ do
        ruleCatches dnfVersionPinned "RUN dnf install -y tomcat && dnf clean all"
        ruleCatchesNot dnfVersionPinned "RUN dnf install -y tomcat-9.0.1 && dnf clean all"
        ruleCatchesNot dnfVersionPinned "RUN notdnf install tomcat"
        onBuildRuleCatches dnfVersionPinned "RUN dnf install -y tomcat && dnf clean all"
        onBuildRuleCatchesNot dnfVersionPinned "RUN dnf install -y tomcat-9.0.1 && dnf clean all"
        onBuildRuleCatchesNot dnfVersionPinned "RUN notdnf install tomcat"
      it "dnf no clean all" $ do
        ruleCatches dnfCleanup "RUN dnf install -y mariadb-10.4"
        ruleCatchesNot dnfCleanup "RUN dnf install -y mariadb-10.4 && dnf clean all"
        ruleCatchesNot dnfCleanup "RUN notdnf install mariadb"
        onBuildRuleCatches dnfCleanup "RUN dnf install -y mariadb-10.4"
        onBuildRuleCatchesNot dnfCleanup "RUN dnf install -y mariadb-10.4 && dnf clean all"
        onBuildRuleCatchesNot dnfCleanup "RUN notdnf install mariadb"
      it "dnf non-interactive" $ do
        ruleCatches dnfYes "RUN dnf install httpd-2.4.24 && dnf clean all"
        ruleCatchesNot dnfYes "RUN dnf install -y httpd-2.4.24 && dnf clean all"
        ruleCatchesNot dnfYes "RUN notdnf install httpd"
        onBuildRuleCatches dnfYes "RUN dnf install httpd-2.4.24 && dnf clean all"
        onBuildRuleCatchesNot dnfYes "RUN dnf install -y httpd-2.4.24 && dnf clean all"
        onBuildRuleCatchesNot dnfYes "RUN notdnf install httpd"
    --
    describe "apt-get rules" $ do
      it "apt" $
        let dockerFile =
              [ "FROM ubuntu",
                "RUN apt install python"
              ]
         in do
              ruleCatches noApt $ Text.unlines dockerFile
              onBuildRuleCatches noApt $ Text.unlines dockerFile
      it "apt-get upgrade" $ do
        ruleCatches noAptGetUpgrade "RUN apt-get update && apt-get upgrade"
        onBuildRuleCatches noAptGetUpgrade "RUN apt-get update && apt-get upgrade"
      it "apt-get dist-upgrade" $ do
        ruleCatches noAptGetUpgrade "RUN apt-get update && apt-get dist-upgrade"
        onBuildRuleCatches noAptGetUpgrade "RUN apt-get update && apt-get dist-upgrade"
      it "apt-get version pinning" $ do
        ruleCatches aptGetVersionPinned "RUN apt-get update && apt-get install python"
        onBuildRuleCatches aptGetVersionPinned "RUN apt-get update && apt-get install python"
      it "apt-get no cleanup" $
        let dockerFile =
              [ "FROM scratch",
                "RUN apt-get update && apt-get install python"
              ]
         in do
              ruleCatches aptGetCleanup $ Text.unlines dockerFile
              onBuildRuleCatches aptGetCleanup $ Text.unlines dockerFile
      it "apt-get cleanup in stage image" $
        let dockerFile =
              [ "FROM ubuntu as foo",
                "RUN apt-get update && apt-get install python",
                "FROM scratch",
                "RUN echo hey!"
              ]
         in do
              ruleCatchesNot aptGetCleanup $ Text.unlines dockerFile
              onBuildRuleCatchesNot aptGetCleanup $ Text.unlines dockerFile
      it "apt-get no cleanup in last stage" $
        let dockerFile =
              [ "FROM ubuntu as foo",
                "RUN hey!",
                "FROM scratch",
                "RUN apt-get update && apt-get install python"
              ]
         in do
              ruleCatches aptGetCleanup $ Text.unlines dockerFile
              onBuildRuleCatches aptGetCleanup $ Text.unlines dockerFile
      it "apt-get no cleanup in intermediate stage" $
        let dockerFile =
              [ "FROM ubuntu as foo",
                "RUN apt-get update && apt-get install python",
                "FROM foo",
                "RUN hey!"
              ]
         in do
              ruleCatches aptGetCleanup $ Text.unlines dockerFile
              onBuildRuleCatches aptGetCleanup $ Text.unlines dockerFile
      it "no warn apt-get cleanup in intermediate stage that cleans lists" $
        let dockerFile =
              [ "FROM ubuntu as foo",
                "RUN apt-get update && apt-get install python && rm -rf /var/lib/apt/lists/*",
                "FROM foo",
                "RUN hey!"
              ]
         in do
              ruleCatchesNot aptGetCleanup $ Text.unlines dockerFile
              onBuildRuleCatchesNot aptGetCleanup $ Text.unlines dockerFile
      it "no warn apt-get cleanup in intermediate stage when stage not used later" $
        let dockerFile =
              [ "FROM ubuntu as foo",
                "RUN apt-get update && apt-get install python",
                "FROM scratch",
                "RUN hey!"
              ]
         in do
              ruleCatchesNot aptGetCleanup $ Text.unlines dockerFile
              onBuildRuleCatchesNot aptGetCleanup $ Text.unlines dockerFile
      it "apt-get cleanup" $
        let dockerFile =
              [ "FROM scratch",
                "RUN apt-get update && apt-get install python && rm -rf /var/lib/apt/lists/*"
              ]
         in do
              ruleCatchesNot aptGetCleanup $ Text.unlines dockerFile
              onBuildRuleCatchesNot aptGetCleanup $ Text.unlines dockerFile

      it "apt-get pinned chained" $
        let dockerFile =
              [ "RUN apt-get update \\",
                " && apt-get -yqq --no-install-recommends install nodejs=0.10 \\",
                " && rm -rf /var/lib/apt/lists/*"
              ]
         in do
              ruleCatchesNot aptGetVersionPinned $ Text.unlines dockerFile
              onBuildRuleCatchesNot aptGetVersionPinned $ Text.unlines dockerFile

      it "apt-get pinned regression" $
        let dockerFile =
              [ "RUN apt-get update && apt-get install --no-install-recommends -y \\",
                "python-demjson=2.2.2* \\",
                "wget=1.16.1* \\",
                "git=1:2.5.0* \\",
                "ruby=1:2.1.*"
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
              [ "RUN apk add --no-cache flex=2.6.4-r1 \\",
                " && pip install -r requirements.txt"
              ]
         in do
              ruleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
              onBuildRuleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
      it "apk add version pinned regression" $
        let dockerFile =
              [ "RUN apk add --no-cache \\",
                "flex=2.6.4-r1 \\",
                "libffi=3.2.1-r3 \\",
                "python2=2.7.13-r1 \\",
                "libbz2=1.0.6-r5"
              ]
         in do
              ruleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
              onBuildRuleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
      it "apk add version pinned regression - one missed" $
        let dockerFile =
              [ "RUN apk add --no-cache \\",
                "flex=2.6.4-r1 \\",
                "libffi \\",
                "python2=2.7.13-r1 \\",
                "libbz2=1.0.6-r5"
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
              [ "RUN apk add \\",
                "--virtual build-dependencies \\",
                "python-dev=1.1.1 build-base=2.2.2 wget=3.3.3 \\",
                "&& pip install -r requirements.txt \\",
                "&& python setup.py install \\",
                "&& apk del build-dependencies"
              ]
         in do
              ruleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
              onBuildRuleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
      it "apk add with repository without equal sign" $
        let dockerFile =
              [ "RUN apk add --no-cache \\",
                "--repository https://nl.alpinelinux.org/alpine/edge/testing \\",
                "flow=0.78.0-r0"
              ]
         in do
              ruleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
              onBuildRuleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
      it "apk add with repository with equal sign" $
        let dockerFile =
              [ "RUN apk add --no-cache \\",
                "--repository=https://nl.alpinelinux.org/alpine/edge/testing \\",
                "flow=0.78.0-r0"
              ]
         in do
              ruleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
              onBuildRuleCatchesNot apkAddVersionPinned $ Text.unlines dockerFile
      it "apk add with repository (-X) without equal sign" $
        let dockerFile =
              [ "RUN apk add --no-cache \\",
                "-X https://nl.alpinelinux.org/alpine/edge/testing \\",
                "flow=0.78.0-r0"
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
      it "pip3 install from local package" $ do
        ruleCatchesNot pipVersionPinned "RUN pip3 install mypkg.whl"
        ruleCatchesNot pipVersionPinned "RUN pip3 install mypkg.tar.gz"
        onBuildRuleCatchesNot pipVersionPinned "RUN pip3 install mypkg.whl"
        onBuildRuleCatchesNot pipVersionPinned "RUN pip3 install mypkg.tar.gz"
      it "pip install requirements" $ do
        ruleCatchesNot pipVersionPinned "RUN pip install -r requirements.txt"
        onBuildRuleCatchesNot pipVersionPinned "RUN pip install -r requirements.txt"
      it "pip install requirements with long flag" $ do
        ruleCatchesNot pipVersionPinned "RUN pip install --requirement requirements.txt"
        onBuildRuleCatchesNot pipVersionPinned "RUN pip install --requirement requirements.txt"
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
      it "pip version pinned with flag --ignore-installed" $ do
        ruleCatchesNot pipVersionPinned "RUN pip install --ignore-installed MySQL_python==1.2.2"
        onBuildRuleCatchesNot pipVersionPinned "RUN pip install --ignore-installed MySQL_python==1.2.2"
      it "pip version pinned with flag --build" $ do
        ruleCatchesNot pipVersionPinned "RUN pip3 install --build /opt/yamllint yamllint==1.20.0"
        onBuildRuleCatchesNot pipVersionPinned "RUN pip3 install --build /opt/yamllint yamllint==1.20.0"
      it "pip version pinned with flag --prefix" $ do
        ruleCatchesNot pipVersionPinned "RUN pip3 install --prefix /opt/yamllint yamllint==1.20.0"
        onBuildRuleCatchesNot pipVersionPinned "RUN pip3 install --prefix /opt/yamllint yamllint==1.20.0"
      it "pip version pinned with flag --root" $ do
        ruleCatchesNot pipVersionPinned "RUN pip3 install --root /opt/yamllint yamllint==1.20.0"
        onBuildRuleCatchesNot pipVersionPinned "RUN pip3 install --root /opt/yamllint yamllint==1.20.0"
      it "pip version pinned with flag --target" $ do
        ruleCatchesNot pipVersionPinned "RUN pip3 install --target /opt/yamllint yamllint==1.20.0"
        onBuildRuleCatchesNot pipVersionPinned "RUN pip3 install --target /opt/yamllint yamllint==1.20.0"
      it "pip version pinned with flag --trusted-host" $ do
        ruleCatchesNot pipVersionPinned "RUN pip3 install --trusted-host host example==1.2.2"
        onBuildRuleCatchesNot pipVersionPinned "RUN pip3 install --trusted-host host example==1.2.2"
      it "pip version pinned with python -m" $ do
        ruleCatchesNot pipVersionPinned "RUN python -m pip install example==1.2.2"
        onBuildRuleCatchesNot pipVersionPinned "RUN python -m pip install example==1.2.2"
      it "pip version not pinned with python -m" $ do
        ruleCatches pipVersionPinned "RUN python -m pip install example"
        onBuildRuleCatches pipVersionPinned "RUN python -m pip install --index-url url example"
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
      it "pip install --index-url" $ do
        ruleCatchesNot
          pipVersionPinned
          "RUN pip install --index-url https://eg.com/foo foobar==1.0.0"
        onBuildRuleCatchesNot
          pipVersionPinned
          "RUN pip install --index-url https://eg.com/foo foobar==1.0.0"
      it "pip install index-url with -i flag" $ do
        ruleCatchesNot
          pipVersionPinned
          "RUN pip install --index-url https://eg.com/foo foobar==1.0.0"
        onBuildRuleCatchesNot
          pipVersionPinned
          "RUN pip install --index-url https://eg.com/foo foobar==1.0.0"
      it "pip install --index-url with --extra-index-url" $ do
        ruleCatchesNot
          pipVersionPinned
          "RUN pip install --index-url https://eg.com/foo --extra-index-url https://ex-eg.io/foo foobar==1.0.0"
        onBuildRuleCatchesNot
          pipVersionPinned
          "RUN pip install --index-url https://eg.com/foo --extra-index-url https://ex-eg.io/foo foobar==1.0.0"
      it "pip install no cache dir" $ do
        ruleCatchesNot pipVersionPinned "RUN pip install MySQL_python==1.2.2 --no-cache-dir"
        onBuildRuleCatchesNot pipVersionPinned "RUN pip install MySQL_python==1.2.2 --no-cache-dir"
      it "pip install constraints file - long version argument" $ do
        ruleCatchesNot pipVersionPinned "RUN pip install pykafka --constraint http://foo.bar.baz"
        onBuildRuleCatchesNot pipVersionPinned "RUN pip install pykafka --constraint http://foo.bar.baz"
      it "pip install constraints file - short version argument" $ do
        ruleCatchesNot pipVersionPinned "RUN pip install pykafka -c http://foo.bar.baz"
        onBuildRuleCatchesNot pipVersionPinned "RUN pip install pykafka -c http://foo.bar.baz"
      it "pip install --index-url with --extra-index-url with basic auth" $ do
        ruleCatchesNot
          pipVersionPinned
          "RUN pip install --index-url https://user:pass@eg.com/foo --extra-index-url https://user:pass@ex-eg.io/foo foobar==1.0.0"
        onBuildRuleCatchesNot
          pipVersionPinned
          "RUN pip install --index-url https://user:pass@eg.com/foo --extra-index-url https://user:pass@ex-eg.io/foo foobar==1.0.0"

    --
    describe "pip cache dir" $ do
      it "pip2 --no-cache-dir not used" $ do
        ruleCatches pipNoCacheDir           "RUN pip2 install MySQL_python"
        onBuildRuleCatches pipNoCacheDir    "RUN pip2 install MySQL_python"
      it "pip3 --no-cache-dir not used" $ do
        ruleCatches pipNoCacheDir           "RUN pip3 install MySQL_python"
        onBuildRuleCatches pipNoCacheDir    "RUN pip3 install MySQL_python"
      it "pip --no-cache-dir not used" $ do
        ruleCatches pipNoCacheDir           "RUN pip install MySQL_python"
        onBuildRuleCatches pipNoCacheDir    "RUN pip install MySQL_python"
      it "pip2 --no-cache-dir used" $ do
        ruleCatchesNot pipNoCacheDir        "RUN pip2 install MySQL_python --no-cache-dir"
        onBuildRuleCatchesNot pipNoCacheDir "RUN pip2 install MySQL_python --no-cache-dir"
      it "pip3 --no-cache-dir used" $ do
        ruleCatchesNot pipNoCacheDir        "RUN pip3 install --no-cache-dir MySQL_python"
        onBuildRuleCatchesNot pipNoCacheDir "RUN pip3 install --no-cache-dir MySQL_python"
      it "pip --no-cache-dir used" $ do
        ruleCatchesNot pipNoCacheDir        "RUN pip install MySQL_python --no-cache-dir"
        onBuildRuleCatchesNot pipNoCacheDir "RUN pip install MySQL_python --no-cache-dir"
      it "don't match on pipx" $ do
        ruleCatchesNot pipNoCacheDir        "RUN pipx install software"
        onBuildRuleCatchesNot pipNoCacheDir "Run pipx install software"
      it "don't match on pipenv" $ do
        ruleCatchesNot pipNoCacheDir        "RUN pipenv install library"
        onBuildRuleCatchesNot pipNoCacheDir "RUN pipenv install library"
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
      it "version does not have to be pinned for tarball suffix .tar" $ do
        ruleCatchesNot npmVersionPinned "RUN npm install package-v1.2.3.tar"
        onBuildRuleCatchesNot npmVersionPinned "RUN npm install package-v1.2.3.tar"
      it "version does not have to be pinned for tarball suffix .tar.gz" $ do
        ruleCatchesNot npmVersionPinned "RUN npm install package-v1.2.3.tar.gz"
        onBuildRuleCatchesNot npmVersionPinned "RUN npm install package-v1.2.3.tar.gz"
      it "version does not have to be pinned for tarball suffix .tgz" $ do
        ruleCatchesNot npmVersionPinned "RUN npm install package-v1.2.3.tgz"
        onBuildRuleCatchesNot npmVersionPinned "RUN npm install package-v1.2.3.tgz"
      it "version does not have to be pinned for folder - absolute path" $ do
        ruleCatchesNot npmVersionPinned "RUN npm install /folder"
        onBuildRuleCatchesNot npmVersionPinned "RUN npm install /folder"
      it "version does not have to be pinned for folder - relative path from current folder" $ do
        ruleCatchesNot npmVersionPinned "RUN npm install ./folder"
        onBuildRuleCatchesNot npmVersionPinned "RUN npm install ./folder"
      it "version does not have to be pinned for folder - relative path to parent folder" $ do
        ruleCatchesNot npmVersionPinned "RUN npm install ../folder"
        onBuildRuleCatchesNot npmVersionPinned "RUN npm install ../folder"
      it "version does not have to be pinned for folder - relative path from home" $ do
        ruleCatchesNot npmVersionPinned "RUN npm install ~/folder"
        onBuildRuleCatchesNot npmVersionPinned "RUN npm install ~/folder"
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
      it "npm run install is fine" $ do
        ruleCatchesNot
          npmVersionPinned
          "RUN npm run --crazy install"
        onBuildRuleCatchesNot
          npmVersionPinned
          "RUN npm run --crazy install"

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
    --
    describe "Shellcheck" $ do
      it "runs shellchek on RUN instructions" $ do
        ruleCatches shellcheck "RUN echo $MISSING_QUOTES"
        onBuildRuleCatches shellcheck "RUN echo $MISSING_QUOTES"
      it "not warns on valid scripts" $ do
        ruleCatchesNot shellcheck "RUN echo foo"
        onBuildRuleCatchesNot shellcheck "RUN echo foo"

      it "Does not complain on default env vars" $
        let dockerFile =
              Text.unlines
                [ "RUN echo \"$HTTP_PROXY\"",
                  "RUN echo \"$http_proxy\"",
                  "RUN echo \"$HTTPS_PROXY\"",
                  "RUN echo \"$https_proxy\"",
                  "RUN echo \"$FTP_PROXY\"",
                  "RUN echo \"$ftp_proxy\"",
                  "RUN echo \"$NO_PROXY\"",
                  "RUN echo \"$no_proxy\""
                ]
         in do
              ruleCatchesNot shellcheck dockerFile
              onBuildRuleCatchesNot shellcheck dockerFile

      it "Complain on missing env vars" $
        let dockerFile =
              Text.unlines
                [ "RUN echo \"$RTTP_PROXY\""
                ]
         in do
              ruleCatches shellcheck dockerFile
              onBuildRuleCatches shellcheck dockerFile

      it "Is aware of ARGS and ENV" $
        let dockerFile =
              Text.unlines
                [ "ARG foo=bar",
                  "ARG another_foo",
                  "ENV bar=10 baz=20",
                  "RUN echo \"$foo\"",
                  "RUN echo \"$another_foo\"",
                  "RUN echo \"$bar\"",
                  "RUN echo \"$baz\""
                ]
         in do
              ruleCatchesNot shellcheck dockerFile
              onBuildRuleCatchesNot shellcheck dockerFile

      it "Resets env vars after a FROM" $
        let dockerFile =
              Text.unlines
                [ "ARG foo=bar",
                  "ARG another_foo",
                  "ENV bar=10 baz=20",
                  "FROM debian",
                  "RUN echo \"$foo\""
                ]
         in do
              ruleCatches shellcheck dockerFile
              onBuildRuleCatches shellcheck dockerFile

      it "Defaults the shell to sh" $
        let dockerFile =
              Text.unlines
                [ "RUN echo $RANDOM"
                ]
         in do
              ruleCatches shellcheck dockerFile
              onBuildRuleCatches shellcheck dockerFile

      it "Can change the shell check to bash" $
        let dockerFile =
              Text.unlines
                [ "SHELL [\"/bin/bash\", \"-eo\", \"pipefail\", \"-c\"]",
                  "RUN echo $RANDOM"
                ]
         in do
              ruleCatchesNot shellcheck dockerFile
              onBuildRuleCatchesNot shellcheck dockerFile

      it "Resets the SHELL to sh after a FROM" $
        let dockerFile =
              Text.unlines
                [ "SHELL [\"/bin/bash\", \"-eo\", \"pipefail\", \"-c\"]",
                  "FROM debian",
                  "RUN echo $RANDOM"
                ]
         in do
              ruleCatches shellcheck dockerFile
              onBuildRuleCatches shellcheck dockerFile

      it "Does not complain on ash shell" $
        let dockerFile =
              Text.unlines
                [ "SHELL [\"/bin/ash\", \"-o\", \"pipefail\", \"-c\"]",
                  "RUN echo hello"
                ]
         in do
              ruleCatchesNot shellcheck dockerFile
              onBuildRuleCatchesNot shellcheck dockerFile

      it "Does not complain on powershell" $
        let dockerFile =
              Text.unlines
                [ "SHELL [\"pwsh\", \"-c\"]",
                  "RUN Get-Variable PSVersionTable | Select-Object -ExpandProperty Value"
                ]
         in do
              ruleCatchesNot shellcheck dockerFile
              onBuildRuleCatchesNot shellcheck dockerFile
    --
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
      it "apt-get with assume-yes" $ do
        ruleCatchesNot aptGetYes "RUN apt-get --assume-yes install python"
        onBuildRuleCatchesNot aptGetYes "RUN apt-get --assume-yes install python"
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
      it "apt-get no install recommends via option" $ do
        ruleCatchesNot aptGetNoRecommends "RUN apt-get -o APT::Install-Recommends=false install python"
        onBuildRuleCatchesNot aptGetNoRecommends "RUN apt-get -o APT::Install-Recommends=false install python"
      it "apt-get version" $ do
        ruleCatchesNot aptGetVersionPinned "RUN apt-get install -y python=1.2.2"
        onBuildRuleCatchesNot aptGetVersionPinned "RUN apt-get install -y python=1.2.2"
      it "apt-get version" $ do
        ruleCatchesNot aptGetVersionPinned "RUN apt-get install ./wkhtmltox_0.12.5-1.bionic_amd64.deb"
        onBuildRuleCatchesNot aptGetVersionPinned "RUN apt-get install ./wkhtmltox_0.12.5-1.bionic_amd64.deb"
      it "apt-get pinned" $ do
        ruleCatchesNot
          aptGetVersionPinned
          "RUN apt-get -y --no-install-recommends install nodejs=0.10"
        onBuildRuleCatchesNot
          aptGetVersionPinned
          "RUN apt-get -y --no-install-recommends install nodejs=0.10"
      it "apt-get tolerate target-release" $
        let dockerFile =
              [ "RUN set -e &&\\",
                " echo \"deb http://http.debian.net/debian jessie-backports main\" \
                \> /etc/apt/sources.list.d/jessie-backports.list &&\\",
                " apt-get update &&\\",
                " apt-get install -y --no-install-recommends -t jessie-backports \
                \openjdk-8-jdk=8u131-b11-1~bpo8+1 &&\\",
                " rm -rf /var/lib/apt/lists/*"
              ]
         in do
              ruleCatchesNot aptGetVersionPinned $ Text.unlines dockerFile
              onBuildRuleCatchesNot aptGetVersionPinned $ Text.unlines dockerFile

      it "has maintainer" $ ruleCatches hasNoMaintainer "FROM debian\nMAINTAINER Lukas"
      it "has maintainer first" $ ruleCatches hasNoMaintainer "MAINTAINER Lukas\nFROM DEBIAN"
      it "has no maintainer" $ ruleCatchesNot hasNoMaintainer "FROM debian"
      it "using add" $ ruleCatches copyInsteadAdd "ADD file /usr/src/app/"

      it "many cmds" $
        let dockerFile =
              [ "FROM debian",
                "CMD bash",
                "RUN foo",
                "CMD another"
              ]
         in ruleCatches multipleCmds $ Text.unlines dockerFile

      it "single cmds, different stages" $
        let dockerFile =
              [ "FROM debian as distro1",
                "CMD bash",
                "RUN foo",
                "FROM debian as distro2",
                "CMD another"
              ]
         in ruleCatchesNot multipleCmds $ Text.unlines dockerFile

      it "many cmds, different stages" $
        let dockerFile =
              [ "FROM debian as distro1",
                "CMD bash",
                "RUN foo",
                "CMD another",
                "FROM debian as distro2",
                "CMD another"
              ]
         in ruleCatches multipleCmds $ Text.unlines dockerFile

      it "single cmd" $ ruleCatchesNot multipleCmds "CMD /bin/true"
      it "no cmd" $ ruleCatchesNot multipleEntrypoints "FROM busybox"

      it "many entrypoints" $
        let dockerFile =
              [ "FROM debian",
                "ENTRYPOINT bash",
                "RUN foo",
                "ENTRYPOINT another"
              ]
         in ruleCatches multipleEntrypoints $ Text.unlines dockerFile

      it "single entrypoint, different stages" $
        let dockerFile =
              [ "FROM debian as distro1",
                "ENTRYPOINT bash",
                "RUN foo",
                "FROM debian as distro2",
                "ENTRYPOINT another"
              ]
         in ruleCatchesNot multipleEntrypoints $ Text.unlines dockerFile

      it "many entrypoints, different stages" $
        let dockerFile =
              [ "FROM debian as distro1",
                "ENTRYPOINT bash",
                "RUN foo",
                "ENTRYPOINT another",
                "FROM debian as distro2",
                "ENTRYPOINT another"
              ]
         in ruleCatches multipleEntrypoints $ Text.unlines dockerFile

      it "single entry" $ ruleCatchesNot multipleEntrypoints "ENTRYPOINT /bin/true"
      it "no entry" $ ruleCatchesNot multipleEntrypoints "FROM busybox"
      it "workdir relative" $ ruleCatches absoluteWorkdir "WORKDIR relative/dir"
      it "workdir absolute" $ ruleCatchesNot absoluteWorkdir "WORKDIR /usr/local"
      it "workdir variable" $ ruleCatchesNot absoluteWorkdir "WORKDIR ${work}"
      it "workdir relative single quotes" $ ruleCatches absoluteWorkdir "WORKDIR \'relative/dir\'"
      it "workdir absolute single quotes" $ ruleCatchesNot absoluteWorkdir "WORKDIR \'/usr/local\'"
      -- no test for variable/single quotes since the variable would not expand.
      it "workdir relative double quotes" $ ruleCatches absoluteWorkdir "WORKDIR \"relative/dir\""
      it "workdir absolute double quotes" $ ruleCatchesNot absoluteWorkdir "WORKDIR \"/usr/local\""
      it "workdir variable double quotes" $ ruleCatchesNot absoluteWorkdir "WORKDIR \"${dir}\""
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
              [ "FROM scratch",
                "COPY --from=build foo .",
                "FROM node as build",
                "RUN baz"
              ]
         in ruleCatches copyFromExists $ Text.unlines dockerFile
      it "don't warn on correctly defined aliases" $
        let dockerFile =
              [ "FROM scratch as build",
                "RUN foo",
                "FROM node",
                "COPY --from=build foo .",
                "RUN baz"
              ]
         in ruleCatchesNot copyFromExists $ Text.unlines dockerFile
    --
    describe "copy from own FROM" $ do
      it "warn on copying from your the same FROM" $
        let dockerFile =
              [ "FROM node as foo",
                "COPY --from=foo bar ."
              ]
         in ruleCatches copyFromAnother $ Text.unlines dockerFile
      it "don't warn on copying form other sources" $
        let dockerFile =
              [ "FROM scratch as build",
                "RUN foo",
                "FROM node as run",
                "COPY --from=build foo .",
                "RUN baz"
              ]
         in ruleCatchesNot copyFromAnother $ Text.unlines dockerFile
    --
    describe "Duplicate aliases" $ do
      it "warn on duplicate aliases" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN something",
                "FROM scratch as foo",
                "RUN something"
              ]
         in ruleCatches fromAliasUnique $ Text.unlines dockerFile
      it "don't warn on unique aliases" $
        let dockerFile =
              [ "FROM scratch as build",
                "RUN foo",
                "FROM node as run",
                "RUN baz"
              ]
         in ruleCatchesNot fromAliasUnique $ Text.unlines dockerFile
    --
    describe "format error" $
      it "display error after line pos" $ do
        let ast = parseText "FOM debian:jessie"
            expectedMsg =
              "<string>:1:1 unexpected 'F' expecting '#', ADD, ARG, CMD, COPY, ENTRYPOINT, "
                <> "ENV, EXPOSE, FROM, HEALTHCHECK, LABEL, MAINTAINER, ONBUILD, RUN, SHELL, STOPSIGNAL, "
                <> "USER, VOLUME, WORKDIR, or end of input "
        case ast of
          Left err -> assertEqual "Unexpected error msg" expectedMsg (formatError err)
          Right _ -> assertFailure "AST should fail parsing"
    --
    describe "Rules can be ignored with inline comments" $ do
      it "ignores single rule" $
        let dockerFile =
              [ "FROM ubuntu",
                "# hadolint ignore=DL3002",
                "USER root"
              ]
         in ruleCatchesNot noRootUser $ Text.unlines dockerFile
      it "ignores only the given rule" $
        let dockerFile =
              [ "FROM scratch",
                "# hadolint ignore=DL3001",
                "USER root"
              ]
         in ruleCatches noRootUser $ Text.unlines dockerFile
      it "ignores only the given rule, when multiple passed" $
        let dockerFile =
              [ "FROM scratch",
                "# hadolint ignore=DL3001,DL3002",
                "USER root"
              ]
         in ruleCatchesNot noRootUser $ Text.unlines dockerFile
      it "ignores the rule only if directly above the instruction" $
        let dockerFile =
              [ "# hadolint ignore=DL3001,DL3002",
                "FROM ubuntu",
                "USER root"
              ]
         in ruleCatches noRootUser $ Text.unlines dockerFile
      it "won't ignore the rule if passed invalid rule names" $
        let dockerFile =
              [ "FROM scratch",
                "# hadolint ignore=crazy,DL3002",
                "USER root"
              ]
         in ruleCatches noRootUser $ Text.unlines dockerFile
      it "ignores multiple rules correctly, even with some extra whitespace" $
        let dockerFile =
              [ "FROM node as foo",
                "# hadolint ignore=DL3023, DL3021",
                "COPY --from=foo bar baz ."
              ]
         in do
              ruleCatchesNot copyFromAnother $ Text.unlines dockerFile
              ruleCatchesNot copyEndingSlash $ Text.unlines dockerFile
    --
    describe "JSON notation in ENTRYPOINT and CMD" $ do
      it "warn on ENTRYPOINT" $
        let dockerFile =
              [ "FROM node as foo",
                "ENTRYPOINT something"
              ]
         in ruleCatches useJsonArgs $ Text.unlines dockerFile
      it "don't warn on ENTRYPOINT json notation" $
        let dockerFile =
              [ "FROM scratch as build",
                "ENTRYPOINT [\"foo\", \"bar\"]"
              ]
         in ruleCatchesNot useJsonArgs $ Text.unlines dockerFile
      it "warn on CMD" $
        let dockerFile =
              [ "FROM node as foo",
                "CMD something"
              ]
         in ruleCatches useJsonArgs $ Text.unlines dockerFile
      it "don't warn on CMD json notation" $
        let dockerFile =
              [ "FROM scratch as build",
                "CMD [\"foo\", \"bar\"]",
                "CMD [ \"foo\", \"bar\" ]"
              ]
         in ruleCatchesNot useJsonArgs $ Text.unlines dockerFile

    --
    describe "Detects missing pipefail option" $ do
      it "warn on missing pipefail" $
        let dockerFile =
              [ "FROM scratch",
                "RUN wget -O - https://some.site | wc -l > /number"
              ]
         in ruleCatches usePipefail $ Text.unlines dockerFile
      it "don't warn on commands with no pipes" $
        let dockerFile =
              [ "FROM scratch as build",
                "RUN wget -O - https://some.site && wc -l file > /number"
              ]
         in ruleCatchesNot usePipefail $ Text.unlines dockerFile
      it "don't warn on commands with pipes and the pipefail option" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"/bin/bash\", \"-eo\", \"pipefail\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number"
              ]
         in ruleCatchesNot usePipefail $ Text.unlines dockerFile
      it "don't warn on commands with pipes and the pipefail option 2" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"/bin/bash\", \"-e\", \"-o\", \"pipefail\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number"
              ]
         in ruleCatchesNot usePipefail $ Text.unlines dockerFile
      it "don't warn on commands with pipes and the pipefail option 3" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"/bin/bash\", \"-o\", \"errexit\", \"-o\", \"pipefail\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number"
              ]
         in ruleCatchesNot usePipefail $ Text.unlines dockerFile
      it "don't warn on commands with pipes and the pipefail zsh" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"/bin/zsh\", \"-o\", \"pipefail\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number"
              ]
         in ruleCatchesNot usePipefail $ Text.unlines dockerFile
      it "don't warn on powershell" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"pwsh\", \"-c\"]",
                "RUN Get-Variable PSVersionTable | Select-Object -ExpandProperty Value"
              ]
         in ruleCatchesNot usePipefail $ Text.unlines dockerFile
      it "warns when using plain sh" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"/bin/sh\", \"-o\", \"pipefail\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number"
              ]
         in ruleCatches usePipefail $ Text.unlines dockerFile
      it "warn on missing pipefail in the next image" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"/bin/bash\", \"-o\", \"pipefail\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number",
                "FROM scratch as build2",
                "RUN wget -O - https://some.site | wc -l file > /number"
              ]
         in ruleCatches usePipefail $ Text.unlines dockerFile
      it "warn on missing pipefail if next SHELL is not using it" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"/bin/bash\", \"-o\", \"pipefail\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number",
                "SHELL [\"/bin/sh\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number"
              ]
         in ruleCatches usePipefail $ Text.unlines dockerFile
    --
    describe "Allowed docker registries" $ do
      it "warn on non-allowed registry" $
        let dockerFile =
              [ "FROM random.com/debian"
              ]
         in ruleCatches (registryIsAllowed ["docker.io"]) $ Text.unlines dockerFile
      it "don't warn on empty allowed registries" $
        let dockerFile =
              [ "FROM random.com/debian"
              ]
         in ruleCatchesNot (registryIsAllowed []) $ Text.unlines dockerFile
      it "don't warn on allowed registries" $
        let dockerFile =
              [ "FROM random.com/debian"
              ]
         in ruleCatchesNot (registryIsAllowed ["x.com", "random.com"]) $ Text.unlines dockerFile
      it "doesn't warn on scratch image" $
        let dockerFile =
              [ "FROM scratch"
              ]
         in ruleCatchesNot (registryIsAllowed ["x.com", "random.com"]) $ Text.unlines dockerFile
      it "allows boths all forms of docker.io" $
        let dockerFile =
              [ "FROM ubuntu:18.04 AS builder1",
                "FROM zemanlx/ubuntu:18.04 AS builder2",
                "FROM docker.io/zemanlx/ubuntu:18.04 AS builder3"
              ]
         in ruleCatchesNot (registryIsAllowed ["docker.io"]) $ Text.unlines dockerFile

      it "allows using previous stages" $
        let dockerFile =
              [ "FROM random.com/foo AS builder1",
                "FROM builder1 AS builder2"
              ]
         in ruleCatchesNot (registryIsAllowed ["random.com"]) $ Text.unlines dockerFile
    --
    describe "Wget or Curl" $ do
      it "warns when using both wget and curl" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz",
                "RUN curl localhost"
              ]
         in ruleCatches wgetOrCurl $ Text.unlines dockerFile
      it "warns when using both wget and curl in same instruction" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz && curl localhost"
              ]
         in ruleCatches wgetOrCurl $ Text.unlines dockerFile
      it "does not warn when using only wget" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz"
              ]
         in ruleCatchesNot wgetOrCurl $ Text.unlines dockerFile
      it "does not warn when using both curl and wget in different stages" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz",
                "FROM scratch",
                "RUN curl localhost"
              ]
         in ruleCatchesNot wgetOrCurl $ Text.unlines dockerFile
      it "does not warns when using both, on a single stage" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz",
                "RUN curl localhost",
                "FROM scratch",
                "RUN curl localhost"
              ]
         in ruleCatches wgetOrCurl $ Text.unlines dockerFile
      it "only warns on the relevant RUN instruction" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz",
                "RUN curl my.xyz",
                "RUN echo hello"
              ]
         in assertChecks
              wgetOrCurl
              (Text.unlines dockerFile)
              ( \checks ->
                  assertBool
                    "Expecting warnings only in 1 RUN instruction"
                    (length checks == 1)
              )
      it "only warns on many relevant RUN instructions" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz",
                "RUN curl my.xyz",
                "RUN echo hello",
                "RUN wget foo.com"
              ]
         in assertChecks
              wgetOrCurl
              (Text.unlines dockerFile)
              ( \checks ->
                  assertBool
                    "Expecting warnings only in 2 RUN instructions"
                    (length checks == 2)
              )
    --
    describe "ONBUILD" $ do
      it "error when using `ONBUILD` within `ONBUILD`" $
        let dockerFile =
              [ "ONBUILD ONBUILD RUN anything"
              ]
        in ruleCatches noIllegalInstructionInOnbuild $ Text.unlines dockerFile
      it "error when using `FROM` within `ONBUILD`" $
        let dockerFile =
              [ "ONBUILD FROM debian:buster"
              ]
        in ruleCatches noIllegalInstructionInOnbuild $ Text.unlines dockerFile
      it "error when using `MAINTAINER` within `ONBUILD`" $
        let dockerFile =
              [ "ONBUILD MAINTAINER \"BoJack Horseman\""
              ]
        in ruleCatches noIllegalInstructionInOnbuild $ Text.unlines dockerFile
      it "ok with `ADD`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD ADD anything anywhere"
      it "ok with `USER`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD USER anything"
      it "ok with `LABEL`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD LABEL bla=\"blubb\""
      it "ok with `STOPSIGNAL`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD STOPSIGNAL anything"
      it "ok with `COPY`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD COPY anything anywhere"
      it "ok with `RUN`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD RUN anything"
      it "ok with `CMD`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD CMD anything"
      it "ok with `SHELL`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD SHELL anything"
      it "ok with `WORKDIR`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD WORKDIR anything"
      it "ok with `EXPOSE`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD EXPOSE 69"
      it "ok with `VOLUME`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD VOLUME anything"
      it "ok with `ENTRYPOINT`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD ENTRYPOINT anything"
      it "ok with `ENV`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD ENV MYVAR=\"bla\""
      it "ok with `ARG`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD ARG anything"
      it "ok with `HEALTHCHECK`" $ ruleCatchesNot noIllegalInstructionInOnbuild "ONBUILD HEALTHCHECK NONE"
      it "ok with `FROM` outside of `ONBUILD`" $ ruleCatchesNot noIllegalInstructionInOnbuild "FROM debian:buster"
      it "ok with `MAINTAINER` outside of `ONBUILD`" $ ruleCatchesNot noIllegalInstructionInOnbuild "MAINTAINER \"Some Guy\""
    --
    describe "Selfreferencing `ENV`s" $ do
      it "ok with normal ENV" $
        ruleCatchesNot noSelfreferencingEnv "ENV BLA=\"blubb\"\nENV BLUBB=\"${BLA}/blubb\""
      it "ok with partial match 1" $
        ruleCatchesNot noSelfreferencingEnv "ENV BLA=\"blubb\" BLUBB=\"${FOOBLA}/blubb\""
      it "ok with partial match 2" $
        ruleCatchesNot noSelfreferencingEnv "ENV BLA=\"blubb\" BLUBB=\"${BLAFOO}/blubb\""
      it "ok with partial match 3" $
        ruleCatchesNot noSelfreferencingEnv "ENV BLA=\"blubb\" BLUBB=\"$FOOBLA/blubb\""
      it "ok with partial match 4" $
        ruleCatchesNot noSelfreferencingEnv "ENV BLA=\"blubb\" BLUBB=\"$BLAFOO/blubb\""
      it "fail with partial match 5" $
        ruleCatches noSelfreferencingEnv "ENV BLA=\"blubb\" BLUBB=\"$BLA/$BLAFOO/blubb\""
      it "ok when previously defined in `ARG`" $
        ruleCatchesNot noSelfreferencingEnv "ARG BLA\nENV BLA=${BLA}"
      it "ok when previously defined in `ENV`" $
        ruleCatchesNot noSelfreferencingEnv "ENV BLA blubb\nENV BLA=${BLA}"
      it "ok with referencing a variable on its own right hand side" $
        ruleCatchesNot noSelfreferencingEnv "ENV PATH=/bla:${PATH}"
      it "ok with referencing a variable on its own right side twice in different `ENV`s" $
        ruleCatchesNot noSelfreferencingEnv "ENV PATH=/bla:${PATH}\nENV PATH=/blubb:${PATH}"
      it "fail when referencing a variable on its own right side twice within the same `ENV`" $
        ruleCatches noSelfreferencingEnv "ENV PATH=/bla:${PATH} PATH=/blubb:${PATH}"
      it "fail with selfreferencing with curly braces ENV" $
        ruleCatches noSelfreferencingEnv "ENV BLA=\"blubb\" BLUBB=\"${BLA}/blubb\""
      it "fail with selfreferencing without curly braces ENV" $
        ruleCatches noSelfreferencingEnv "ENV BLA=\"blubb\" BLUBB=\"$BLA/blubb\""
    --
    describe "Regression Tests" $ do
      it "Comments with backslashes at the end are just comments" $
        let dockerFile =
              [ "FROM alpine:3.6",
                "# The following comment makes hadolint still complain about DL4006",
                "# \\",
                "# should solve DL4006",
                "SHELL [\"/bin/sh\", \"-o\", \"pipefail\", \"-c\"]",
                "# RUN with pipe. causes DL4006, but should be fixed by above SHELL",
                "RUN echo \"kaka\" | sed 's/a/o/g' >> /root/afile"
              ]
         in ruleCatches usePipefail $ Text.unlines dockerFile
      it "`ARG` can correctly unset variables" $
        let dockerFile =
              [ "ARG A_WITHOUT_EQ",
                "ARG A_WITH_EQ=",
                "RUN echo bla"
              ]
         in assertChecks
              shellcheck
              (Text.unlines dockerFile)
              (assertBool "No Warnings or Errors should be triggered" . null)

    -- Run tests for the Config module
    ConfigSpec.tests

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

selectChecksWithLines :: [RuleCheck] -> [RuleCheck]
selectChecksWithLines checks = [c | c <- checks, linenumber c <= 0]

-- Assert a failed check exists for rule
ruleCatches :: HasCallStack => Rule -> Text.Text -> Assertion
ruleCatches rule s = assertChecks rule s f
  where
    f checks = do
      when (null checks) $
        assertFailure "I was expecting to catch at least one error"
      assertBool "Incorrect line number for result" $ null $ selectChecksWithLines checks

onBuildRuleCatches :: HasCallStack => Rule -> Text.Text -> Assertion
onBuildRuleCatches rule s = assertOnBuildChecks rule s f
  where
    f checks = do
      when (length checks /= 1) $
        assertFailure (Text.unpack . Text.unlines . formatChecksNoColor $ checks)
      assertBool "Incorrect line number for result" $ null $ selectChecksWithLines checks

ruleCatchesNot :: HasCallStack => Rule -> Text.Text -> Assertion
ruleCatchesNot rule s = assertChecks rule s f
  where
    f checks =
      unless (null checks) $
        assertFailure $
          "Not expecting the following errors: \n"
            ++ (Text.unpack . Text.unlines . formatChecksNoColor $ checks)

onBuildRuleCatchesNot :: HasCallStack => Rule -> Text.Text -> Assertion
onBuildRuleCatchesNot rule s = assertOnBuildChecks rule s f
  where
    f checks =
      unless (null checks) $
        assertFailure $
          "Not expecting the following errors: \n"
            ++ (Text.unpack . Text.unlines . formatChecksNoColor $ checks)

formatChecksNoColor :: Functor f => f RuleCheck -> f Text.Text
formatChecksNoColor checks = formatChecks checks False
