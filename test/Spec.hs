import qualified ConfigSpec
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified DL3045
import Hadolint.Formatter.TTY (formatError)
import qualified Hadolint.Process
import Hadolint.Rule as Rule
import Helpers
import Language.Docker.Parser
import qualified ShellSpec
import Test.HUnit hiding (Label)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    let ?rulesConfig = mempty -- default implicit parameter running the checkers
    describe "FROM rules" $ do
      it "no untagged" $ ruleCatches "DL3006" "FROM debian"
      it "no untagged with name" $ ruleCatches "DL3006" "FROM debian AS builder"
      it "explicit latest" $ ruleCatches "DL3007" "FROM debian:latest"
      it "explicit latest with name" $ ruleCatches "DL3007" "FROM debian:latest AS builder"
      it "explicit tagged" $ ruleCatchesNot "DL3007" "FROM debian:jessie"
      it "explicit platform flag" $ ruleCatches "DL3029" "FROM --platform=linux debian:jessie"
      it "no platform flag" $ ruleCatchesNot "DL3029" "FROM debian:jessie"
      it "explicit SHA" $
        ruleCatchesNot
          "DL3007"
          "FROM hub.docker.io/debian@sha256:\
          \7959ed6f7e35f8b1aaa06d1d8259d4ee25aa85a086d5c125480c333183f9deeb"
      it "explicit tagged with name" $
        ruleCatchesNot "DL3007" "FROM debian:jessie AS builder"
      it "untagged digest is not an error" $
        ruleCatchesNot "DL3006" "FROM ruby@sha256:f1dbca0f5dbc9"
      it "untagged digest is not an error" $
        ruleCatchesNot "DL3006" "FROM ruby:2"
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
              ruleCatchesNot "DL3006" $ Text.unlines dockerFile
              onBuildRuleCatchesNot "DL3006" $ Text.unlines dockerFile
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
              ruleCatches "DL3006" $ Text.unlines dockerFile
              onBuildRuleCatches "DL3006" $ Text.unlines dockerFile
    --
    describe "no root or sudo rules" $ do
      it "sudo" $ do
        ruleCatches "DL3004" "RUN sudo apt-get update"
        onBuildRuleCatches "DL3004" "RUN sudo apt-get update"

      it "last user should not be root" $
        let dockerFile =
              [ "FROM scratch",
                "USER root"
              ]
         in ruleCatches "DL3002" $ Text.unlines dockerFile

      it "no root" $
        let dockerFile =
              [ "FROM scratch",
                "USER foo"
              ]
         in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

      it "no root UID" $
        let dockerFile =
              [ "FROM scratch",
                "USER 0"
              ]
         in ruleCatches "DL3002" $ Text.unlines dockerFile

      it "no root:root" $
        let dockerFile =
              [ "FROM scratch",
                "USER root:root"
              ]
         in ruleCatches "DL3002" $ Text.unlines dockerFile

      it "no UID:GID" $
        let dockerFile =
              [ "FROM scratch",
                "USER 0:0"
              ]
         in ruleCatches "DL3002" $ Text.unlines dockerFile

      it "can switch back to non root" $
        let dockerFile =
              [ "FROM scratch",
                "USER root",
                "RUN something",
                "USER foo"
              ]
         in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

      it "warns on transitive root user" $
        let dockerFile =
              [ "FROM debian as base",
                "USER root",
                "RUN something",
                "FROM base",
                "RUN something else"
              ]
         in ruleCatches "DL3002" $ Text.unlines dockerFile

      it "warns on multiple stages" $
        let dockerFile =
              [ "FROM debian as base",
                "USER root",
                "RUN something",
                "FROM scratch",
                "USER foo",
                "RUN something else"
              ]
         in ruleCatches "DL3002" $ Text.unlines dockerFile

      it "does not warn when switching in multiple stages" $
        let dockerFile =
              [ "FROM debian as base",
                "USER root",
                "RUN something",
                "USER foo",
                "FROM scratch",
                "RUN something else"
              ]
         in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

      it "install sudo" $ do
        ruleCatchesNot "DL3004" "RUN apt-get install sudo"
        onBuildRuleCatchesNot "DL3004" "RUN apt-get install sudo"
      it "sudo chained programs" $ do
        ruleCatches "DL3004" "RUN apt-get update && sudo apt-get install"
        onBuildRuleCatches "DL3004" "RUN apt-get update && sudo apt-get install"
    --
    describe "invalid CMD rules" $ do
      it "invalid cmd" $ do
        ruleCatches "DL3001" "RUN top"
        onBuildRuleCatches "DL3001" "RUN top"
      it "install ssh" $ do
        ruleCatchesNot "DL3001" "RUN apt-get install ssh"
        onBuildRuleCatchesNot "DL3001" "RUN apt-get install ssh"
    --
    describe "gem" $
      describe "version pinning" $ do
        describe "i" $ do
          it "unpinned" $ do
            ruleCatches "DL3028" "RUN gem i bundler"
            onBuildRuleCatches "DL3028" "RUN gem i bundler"
          it "pinned" $ do
            ruleCatchesNot "DL3028" "RUN gem i bundler:1"
            onBuildRuleCatchesNot "DL3028" "RUN gem i bundler:1"
          it "multi" $ do
            ruleCatches "DL3028" "RUN gem i bunlder:1 nokogiri"
            onBuildRuleCatches "DL3028" "RUN gem i bunlder:1 nokogiri"
            ruleCatchesNot "DL3028" "RUN gem i bunlder:1 nokogirii:1"
            onBuildRuleCatchesNot "DL3028" "RUN gem i bunlder:1 nokogiri:1"
        describe "install" $ do
          it "unpinned" $ do
            ruleCatches "DL3028" "RUN gem install bundler"
            onBuildRuleCatches "DL3028" "RUN gem install bundler"
          it "pinned" $ do
            ruleCatchesNot "DL3028" "RUN gem install bundler:1"
            onBuildRuleCatchesNot "DL3028" "RUN gem install bundler:1"
          it "does not warn on -v" $ do
            ruleCatchesNot "DL3028" "RUN gem install bundler -v '2.0.1'"
            onBuildRuleCatchesNot "DL3028" "RUN gem install bundler -v '2.0.1'"
          it "does not warn on --version without =" $ do
            ruleCatchesNot "DL3028" "RUN gem install bundler --version '2.0.1'"
            onBuildRuleCatchesNot "DL3028" "RUN gem install bundler --version '2.0.1'"
          it "does not warn on --version with =" $ do
            ruleCatchesNot "DL3028" "RUN gem install bundler --version='2.0.1'"
            onBuildRuleCatchesNot "DL3028" "RUN gem install bundler --version='2.0.1'"
          it "does not warn on extra flags" $ do
            ruleCatchesNot "DL3028" "RUN gem install bundler:2.0.1 -- --use-system-libraries=true"
            onBuildRuleCatchesNot "DL3028" "RUN gem install bundler:2.0.1 -- --use-system-libraries=true"
    --
    describe "yum rules" $ do
      it "yum update" $ do
        ruleCatches "DL3031" "RUN yum update"
        ruleCatchesNot "DL3031" "RUN yum install -y httpd-2.4.42 && yum clean all"
        ruleCatchesNot "DL3031" "RUN bash -c `# not even a yum command`"
        onBuildRuleCatches "DL3031" "RUN yum update"
        onBuildRuleCatchesNot "DL3031" "RUN yum install -y httpd-2.4.42 && yum clean all"
        onBuildRuleCatchesNot "DL3031" "RUN bash -c `# not even a yum command`"
      it "yum version pinning" $ do
        ruleCatches "DL3033" "RUN yum install -y tomcat && yum clean all"
        ruleCatchesNot "DL3033" "RUN yum install -y tomcat-9.2 && yum clean all"
        ruleCatchesNot "DL3033" "RUN bash -c `# not even a yum command`"
        onBuildRuleCatches "DL3033" "RUN yum install -y tomcat && yum clean all"
        onBuildRuleCatchesNot "DL3033" "RUN yum install -y tomcat-9.2 && yum clean all"
        onBuildRuleCatchesNot "DL3033" "RUN bash -c `# not even a yum command`"
      it "yum no clean all" $ do
        ruleCatches "DL3032" "RUN yum install -y mariadb-10.4"
        ruleCatchesNot "DL3032" "RUN yum install -y mariadb-10.4 && yum clean all"
        ruleCatchesNot "DL3032" "RUN bash -c `# not even a yum command`"
        onBuildRuleCatches "DL3032" "RUN yum install -y mariadb-10.4"
        onBuildRuleCatchesNot "DL3032" "RUN yum install -y mariadb-10.4 && yum clean all"
        onBuildRuleCatchesNot "DL3032" "RUN bash -c `# not even a yum command`"
      it "yum non-interactive" $ do
        ruleCatches "DL3030" "RUN yum install httpd-2.4.24 && yum clean all"
        ruleCatchesNot "DL3030" "RUN yum install -y httpd-2.4.24 && yum clean all"
        ruleCatchesNot "DL3030" "RUN bash -c `# not even a yum command`"
        onBuildRuleCatches "DL3030" "RUN yum install httpd-2.4.24 && yum clean all"
        onBuildRuleCatchesNot "DL3030" "RUN yum install -y httpd-2.4.24 && yum clean all"
        onBuildRuleCatchesNot "DL3030" "RUN bash -c `# not even a yum command`"
    --
    describe "zypper rules" $ do
      it "zupper update" $ do
        ruleCatches "DL3035" "RUN zypper update"
        ruleCatches "DL3035" "RUN zypper up"
        ruleCatches "DL3035" "RUN zypper dist-upgrade"
        ruleCatches "DL3035" "RUN zypper dup"
        onBuildRuleCatches "DL3035" "RUN zypper update"
        onBuildRuleCatches "DL3035" "RUN zypper up"
        onBuildRuleCatches "DL3035" "RUN zypper dist-upgrade"
        onBuildRuleCatches "DL3035" "RUN zypper dup"
      it "zypper version pinning" $ do
        -- NOTE: In Haskell strings, '\' has to be escaped. And in shell commands, '>'
        -- and '<' have to be escaped. Hence the double escaping.
        ruleCatches "DL3037" "RUN zypper install -y tomcat && zypper clean"
        ruleCatchesNot "DL3037" "RUN zypper install -y tomcat=9.0.39 && zypper clean"
        ruleCatchesNot "DL3037" "RUN zypper install -y tomcat\\>=9.0 && zypper clean"
        ruleCatchesNot "DL3037" "RUN zypper install -y tomcat\\>9.0 && zypper clean"
        ruleCatchesNot "DL3037" "RUN zypper install -y tomcat\\<=9.0 && zypper clean"
        ruleCatchesNot "DL3037" "RUN zypper install -y tomcat\\<9.0 && zypper clean"
        ruleCatchesNot "DL3037" "RUN zypper install -y tomcat-9.0.39-1.rpm && zypper clean"
        onBuildRuleCatches "DL3037" "RUN zypper install -y tomcat && zypper clean"
        onBuildRuleCatchesNot "DL3037" "RUN zypper install -y tomcat=9.0.39 && zypper clean"
        onBuildRuleCatchesNot "DL3037" "RUN zypper install -y tomcat\\>=9.0 && zypper clean"
        onBuildRuleCatchesNot "DL3037" "RUN zypper install -y tomcat\\>9.0 && zypper clean"
        onBuildRuleCatchesNot "DL3037" "RUN zypper install -y tomcat\\<=9.0 && zypper clean"
        onBuildRuleCatchesNot "DL3037" "RUN zypper install -y tomcat\\<9.0 && zypper clean"
        onBuildRuleCatchesNot "DL3037" "RUN zypper install -y tomcat-9.0.39-1.rpm && zypper clean"
      it "zypper no clean all" $ do
        ruleCatches "DL3036" "RUN zypper install -y mariadb=10.4"
        ruleCatchesNot "DL3036" "RUN zypper install -y mariadb=10.4 && zypper clean"
        ruleCatchesNot "DL3036" "RUN zypper install -y mariadb=10.4 && zypper cc"
        onBuildRuleCatches "DL3036" "RUN zypper install -y mariadb=10.4"
        onBuildRuleCatchesNot "DL3036" "RUN zypper install -y mariadb=10.4 && zypper clean"
        onBuildRuleCatchesNot "DL3036" "RUN zypper install -y mariadb=10.4 && zypper cc"
      it "zypper non-interactive" $ do
        ruleCatches "DL3034" "RUN zypper install httpd=2.4.24 && zypper clean"
        ruleCatchesNot "DL3034" "RUN zypper install -y httpd=2.4.24 && zypper clean"
        ruleCatchesNot "DL3034" "RUN zypper install --no-confirm httpd=2.4.24 && zypper clean"
        onBuildRuleCatches "DL3034" "RUN zypper install httpd=2.4.24 && zypper clean"
        onBuildRuleCatchesNot "DL3034" "RUN zypper install -y httpd=2.4.24 && zypper clean"
        onBuildRuleCatchesNot "DL3034" "RUN zypper install --no-confirm httpd=2.4.24 && zypper clean"
    --
    describe "dnf rules" $ do
      it "dnf update" $ do
        ruleCatches "DL3039" "RUN dnf upgrade"
        onBuildRuleCatches "DL3039" "RUN dnf upgrade"
      it "dnf version pinning" $ do
        ruleCatches "DL3041" "RUN dnf install -y tomcat && dnf clean all"
        ruleCatchesNot "DL3041" "RUN dnf install -y tomcat-9.0.1 && dnf clean all"
        onBuildRuleCatches "DL3041" "RUN dnf install -y tomcat && dnf clean all"
        onBuildRuleCatchesNot "DL3041" "RUN dnf install -y tomcat-9.0.1 && dnf clean all"
      it "dnf no clean all" $ do
        ruleCatches "DL3040" "RUN dnf install -y mariadb-10.4"
        ruleCatchesNot "DL3040" "RUN dnf install -y mariadb-10.4 && dnf clean all"
        onBuildRuleCatches "DL3040" "RUN dnf install -y mariadb-10.4"
        onBuildRuleCatchesNot "DL3040" "RUN dnf install -y mariadb-10.4 && dnf clean all"
      it "dnf non-interactive" $ do
        ruleCatches "DL3038" "RUN dnf install httpd-2.4.24 && dnf clean all"
        ruleCatchesNot "DL3038" "RUN dnf install -y httpd-2.4.24 && dnf clean all"
        onBuildRuleCatches "DL3038" "RUN dnf install httpd-2.4.24 && dnf clean all"
        onBuildRuleCatchesNot "DL3038" "RUN dnf install -y httpd-2.4.24 && dnf clean all"
    --
    describe "dnf rules" $ do
      it "dnf update" $ do
        ruleCatches "DL3039" "RUN dnf upgrade"
        ruleCatches "DL3039" "RUN dnf upgrade-minimal"
        ruleCatchesNot "DL3039" "RUN notdnf upgrade"
        onBuildRuleCatches "DL3039" "RUN dnf upgrade"
        onBuildRuleCatches "DL3039" "RUN dnf upgrade-minimal"
        onBuildRuleCatchesNot "DL3039" "RUN notdnf upgrade"
      it "dnf version pinning" $ do
        ruleCatches "DL3041" "RUN dnf install -y tomcat && dnf clean all"
        ruleCatchesNot "DL3041" "RUN dnf install -y tomcat-9.0.1 && dnf clean all"
        ruleCatchesNot "DL3041" "RUN notdnf install tomcat"
        onBuildRuleCatches "DL3041" "RUN dnf install -y tomcat && dnf clean all"
        onBuildRuleCatchesNot "DL3041" "RUN dnf install -y tomcat-9.0.1 && dnf clean all"
        onBuildRuleCatchesNot "DL3041" "RUN notdnf install tomcat"
      it "dnf no clean all" $ do
        ruleCatches "DL3040" "RUN dnf install -y mariadb-10.4"
        ruleCatchesNot "DL3040" "RUN dnf install -y mariadb-10.4 && dnf clean all"
        ruleCatchesNot "DL3040" "RUN notdnf install mariadb"
        onBuildRuleCatches "DL3040" "RUN dnf install -y mariadb-10.4"
        onBuildRuleCatchesNot "DL3040" "RUN dnf install -y mariadb-10.4 && dnf clean all"
        onBuildRuleCatchesNot "DL3040" "RUN notdnf install mariadb"
      it "dnf non-interactive" $ do
        ruleCatches "DL3038" "RUN dnf install httpd-2.4.24 && dnf clean all"
        ruleCatchesNot "DL3038" "RUN dnf install -y httpd-2.4.24 && dnf clean all"
        ruleCatchesNot "DL3038" "RUN notdnf install httpd"
        onBuildRuleCatches "DL3038" "RUN dnf install httpd-2.4.24 && dnf clean all"
        onBuildRuleCatchesNot "DL3038" "RUN dnf install -y httpd-2.4.24 && dnf clean all"
        onBuildRuleCatchesNot "DL3038" "RUN notdnf install httpd"
    --
    describe "apt-get rules" $ do
      it "apt" $
        let dockerFile =
              [ "FROM ubuntu",
                "RUN apt install python"
              ]
         in do
              ruleCatches "DL3027" $ Text.unlines dockerFile
              onBuildRuleCatches "DL3027" $ Text.unlines dockerFile
      it "apt-get upgrade" $ do
        ruleCatches "DL3005" "RUN apt-get update && apt-get upgrade"
        onBuildRuleCatches "DL3005" "RUN apt-get update && apt-get upgrade"
      it "apt-get dist-upgrade" $ do
        ruleCatches "DL3005" "RUN apt-get update && apt-get dist-upgrade"
        onBuildRuleCatches "DL3005" "RUN apt-get update && apt-get dist-upgrade"
      it "apt-get version pinning" $ do
        ruleCatches "DL3008" "RUN apt-get update && apt-get install python"
        onBuildRuleCatches "DL3008" "RUN apt-get update && apt-get install python"
      it "apt-get no cleanup" $
        let dockerFile =
              [ "FROM scratch",
                "RUN apt-get update && apt-get install python"
              ]
         in do
              ruleCatches "DL3009" $ Text.unlines dockerFile
              onBuildRuleCatches "DL3009" $ Text.unlines dockerFile
      it "apt-get cleanup in stage image" $
        let dockerFile =
              [ "FROM ubuntu as foo",
                "RUN apt-get update && apt-get install python",
                "FROM scratch",
                "RUN echo hey!"
              ]
         in do
              ruleCatchesNot "DL3009" $ Text.unlines dockerFile
              onBuildRuleCatches "DL3009" $ Text.unlines dockerFile
      it "apt-get no cleanup in last stage" $
        let dockerFile =
              [ "FROM ubuntu as foo",
                "RUN hey!",
                "FROM scratch",
                "RUN apt-get update && apt-get install python"
              ]
         in do
              ruleCatches "DL3009" $ Text.unlines dockerFile
              onBuildRuleCatches "DL3009" $ Text.unlines dockerFile
      it "apt-get no cleanup in intermediate stage" $
        let dockerFile =
              [ "FROM ubuntu as foo",
                "RUN apt-get update && apt-get install python",
                "FROM foo",
                "RUN hey!"
              ]
         in do
              ruleCatches "DL3009" $ Text.unlines dockerFile
              onBuildRuleCatches "DL3009" $ Text.unlines dockerFile
      it "no warn apt-get cleanup in intermediate stage that cleans lists" $
        let dockerFile =
              [ "FROM ubuntu as foo",
                "RUN apt-get update && apt-get install python && rm -rf /var/lib/apt/lists/*",
                "FROM foo",
                "RUN hey!"
              ]
         in do
              ruleCatchesNot "DL3009" $ Text.unlines dockerFile
              onBuildRuleCatchesNot "DL3009" $ Text.unlines dockerFile
      it "no warn apt-get cleanup in intermediate stage when stage not used later" $
        let dockerFile =
              [ "FROM ubuntu as foo",
                "RUN apt-get update && apt-get install python",
                "FROM scratch",
                "RUN hey!"
              ]
         in do
              ruleCatchesNot "DL3009" $ Text.unlines dockerFile
              onBuildRuleCatches "DL3009" $ Text.unlines dockerFile
      it "apt-get cleanup" $
        let dockerFile =
              [ "FROM scratch",
                "RUN apt-get update && apt-get install python && rm -rf /var/lib/apt/lists/*"
              ]
         in do
              ruleCatchesNot "DL3009" $ Text.unlines dockerFile
              onBuildRuleCatchesNot "DL3009" $ Text.unlines dockerFile

      it "apt-get pinned chained" $
        let dockerFile =
              [ "RUN apt-get update \\",
                " && apt-get -yqq --no-install-recommends install nodejs=0.10 \\",
                " && rm -rf /var/lib/apt/lists/*"
              ]
         in do
              ruleCatchesNot "DL3008" $ Text.unlines dockerFile
              onBuildRuleCatchesNot "DL3008" $ Text.unlines dockerFile

      it "apt-get pinned regression" $
        let dockerFile =
              [ "RUN apt-get update && apt-get install --no-install-recommends -y \\",
                "python-demjson=2.2.2* \\",
                "wget=1.16.1* \\",
                "git=1:2.5.0* \\",
                "ruby=1:2.1.*"
              ]
         in do
              ruleCatchesNot "DL3008" $ Text.unlines dockerFile
              onBuildRuleCatchesNot "DL3008" $ Text.unlines dockerFile

      it "has deprecated maintainer" $
        ruleCatches "DL4000" "FROM busybox\nMAINTAINER hudu@mail.com"
    --
    describe "apk add rules" $ do
      it "apk upgrade" $ do
        ruleCatches "DL3017" "RUN apk update && apk upgrade"
        onBuildRuleCatches "DL3017" "RUN apk update && apk upgrade"
      it "apk add version pinning single" $ do
        ruleCatches "DL3018" "RUN apk add flex"
        onBuildRuleCatches "DL3018" "RUN apk add flex"
      it "apk add no version pinning single" $ do
        ruleCatchesNot "DL3018" "RUN apk add flex=2.6.4-r1"
        onBuildRuleCatchesNot "DL3018" "RUN apk add flex=2.6.4-r1"
      it "apk add version pinned chained" $
        let dockerFile =
              [ "RUN apk add --no-cache flex=2.6.4-r1 \\",
                " && pip install -r requirements.txt"
              ]
         in do
              ruleCatchesNot "DL3018" $ Text.unlines dockerFile
              onBuildRuleCatchesNot "DL3018" $ Text.unlines dockerFile
      it "apk add version pinned regression" $
        let dockerFile =
              [ "RUN apk add --no-cache \\",
                "flex=2.6.4-r1 \\",
                "libffi=3.2.1-r3 \\",
                "python2=2.7.13-r1 \\",
                "libbz2=1.0.6-r5"
              ]
         in do
              ruleCatchesNot "DL3018" $ Text.unlines dockerFile
              onBuildRuleCatchesNot "DL3018" $ Text.unlines dockerFile
      it "apk add version pinned regression - one missed" $
        let dockerFile =
              [ "RUN apk add --no-cache \\",
                "flex=2.6.4-r1 \\",
                "libffi \\",
                "python2=2.7.13-r1 \\",
                "libbz2=1.0.6-r5"
              ]
         in do
              ruleCatches "DL3018" $ Text.unlines dockerFile
              onBuildRuleCatches "DL3018" $ Text.unlines dockerFile
      it "apk add with --no-cache" $ do
        ruleCatches "DL3019" "RUN apk add flex=2.6.4-r1"
        onBuildRuleCatches "DL3019" "RUN apk add flex=2.6.4-r1"
      it "apk add without --no-cache" $ do
        ruleCatchesNot "DL3019" "RUN apk add --no-cache flex=2.6.4-r1"
        onBuildRuleCatchesNot "DL3019" "RUN apk add --no-cache flex=2.6.4-r1"
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
              ruleCatchesNot "DL3018" $ Text.unlines dockerFile
              onBuildRuleCatchesNot "DL3018" $ Text.unlines dockerFile
      it "apk add with repository without equal sign" $
        let dockerFile =
              [ "RUN apk add --no-cache \\",
                "--repository https://nl.alpinelinux.org/alpine/edge/testing \\",
                "flow=0.78.0-r0"
              ]
         in do
              ruleCatchesNot "DL3018" $ Text.unlines dockerFile
              onBuildRuleCatchesNot "DL3018" $ Text.unlines dockerFile
      it "apk add with repository with equal sign" $
        let dockerFile =
              [ "RUN apk add --no-cache \\",
                "--repository=https://nl.alpinelinux.org/alpine/edge/testing \\",
                "flow=0.78.0-r0"
              ]
         in do
              ruleCatchesNot "DL3018" $ Text.unlines dockerFile
              onBuildRuleCatchesNot "DL3018" $ Text.unlines dockerFile
      it "apk add with repository (-X) without equal sign" $
        let dockerFile =
              [ "RUN apk add --no-cache \\",
                "-X https://nl.alpinelinux.org/alpine/edge/testing \\",
                "flow=0.78.0-r0"
              ]
         in do
              ruleCatchesNot "DL3018" $ Text.unlines dockerFile
              onBuildRuleCatchesNot "DL3018" $ Text.unlines dockerFile
    --
    describe "EXPOSE rules" $ do
      it "invalid port" $ ruleCatches "DL3011" "EXPOSE 80000"
      it "valid port" $ ruleCatchesNot "DL3011" "EXPOSE 60000"
    --
    describe "pip pinning" $ do
      it "pip2 version not pinned" $ do
        ruleCatches "DL3013" "RUN pip2 install MySQL_python"
        onBuildRuleCatches "DL3013" "RUN pip2 install MySQL_python"
      it "pip3 version not pinned" $ do
        ruleCatches "DL3013" "RUN pip3 install MySQL_python"
        onBuildRuleCatches "DL3013" "RUN pip2 install MySQL_python"
      it "pip3 version pinned" $ do
        ruleCatchesNot "DL3013" "RUN pip3 install MySQL_python==1.2.2"
        onBuildRuleCatchesNot "DL3013" "RUN pip3 install MySQL_python==1.2.2"
      it "pip3 install from local package" $ do
        ruleCatchesNot "DL3013" "RUN pip3 install mypkg.whl"
        ruleCatchesNot "DL3013" "RUN pip3 install mypkg.tar.gz"
        onBuildRuleCatchesNot "DL3013" "RUN pip3 install mypkg.whl"
        onBuildRuleCatchesNot "DL3013" "RUN pip3 install mypkg.tar.gz"
      it "pip install requirements" $ do
        ruleCatchesNot "DL3013" "RUN pip install -r requirements.txt"
        onBuildRuleCatchesNot "DL3013" "RUN pip install -r requirements.txt"
      it "pip install requirements with long flag" $ do
        ruleCatchesNot "DL3013" "RUN pip install --requirement requirements.txt"
        onBuildRuleCatchesNot "DL3013" "RUN pip install --requirement requirements.txt"
      it "pip install use setup.py" $ do
        ruleCatchesNot "DL3013" "RUN pip install ."
        onBuildRuleCatchesNot "DL3013" "RUN pip install ."
      it "pip version not pinned" $ do
        ruleCatches "DL3013" "RUN pip install MySQL_python"
        onBuildRuleCatches "DL3013" "RUN pip install MySQL_python"
      it "pip version pinned" $ do
        ruleCatchesNot "DL3013" "RUN pip install MySQL_python==1.2.2"
        onBuildRuleCatchesNot "DL3013" "RUN pip install MySQL_python==1.2.2"
      it "pip version pinned with ~= operator" $ do
        ruleCatchesNot "DL3013" "RUN pip install MySQL_python~=1.2.2"
        onBuildRuleCatchesNot "DL3013" "RUN pip install MySQL_python~=1.2.2"
      it "pip version pinned with === operator" $ do
        ruleCatchesNot "DL3013" "RUN pip install MySQL_python===1.2.2"
        onBuildRuleCatchesNot "DL3013" "RUN pip install MySQL_python===1.2.2"
      it "pip version pinned with flag --ignore-installed" $ do
        ruleCatchesNot "DL3013" "RUN pip install --ignore-installed MySQL_python==1.2.2"
        onBuildRuleCatchesNot "DL3013" "RUN pip install --ignore-installed MySQL_python==1.2.2"
      it "pip version pinned with flag --build" $ do
        ruleCatchesNot "DL3013" "RUN pip3 install --build /opt/yamllint yamllint==1.20.0"
        onBuildRuleCatchesNot "DL3013" "RUN pip3 install --build /opt/yamllint yamllint==1.20.0"
      it "pip version pinned with flag --prefix" $ do
        ruleCatchesNot "DL3013" "RUN pip3 install --prefix /opt/yamllint yamllint==1.20.0"
        onBuildRuleCatchesNot "DL3013" "RUN pip3 install --prefix /opt/yamllint yamllint==1.20.0"
      it "pip version pinned with flag --root" $ do
        ruleCatchesNot "DL3013" "RUN pip3 install --root /opt/yamllint yamllint==1.20.0"
        onBuildRuleCatchesNot "DL3013" "RUN pip3 install --root /opt/yamllint yamllint==1.20.0"
      it "pip version pinned with flag --target" $ do
        ruleCatchesNot "DL3013" "RUN pip3 install --target /opt/yamllint yamllint==1.20.0"
        onBuildRuleCatchesNot "DL3013" "RUN pip3 install --target /opt/yamllint yamllint==1.20.0"
      it "pip version pinned with flag --trusted-host" $ do
        ruleCatchesNot "DL3013" "RUN pip3 install --trusted-host host example==1.2.2"
        onBuildRuleCatchesNot "DL3013" "RUN pip3 install --trusted-host host example==1.2.2"
      it "pip version pinned with python -m" $ do
        ruleCatchesNot "DL3013" "RUN python -m pip install example==1.2.2"
        onBuildRuleCatchesNot "DL3013" "RUN python -m pip install example==1.2.2"
      it "pip version not pinned with python -m" $ do
        ruleCatches "DL3013" "RUN python -m pip install example"
        onBuildRuleCatches "DL3013" "RUN python -m pip install --index-url url example"
      it "pip install git" $ do
        ruleCatchesNot
          "DL3013"
          "RUN pip install git+https://github.com/rtfd/r-ext.git@0.6-alpha#egg=r-ext"
        onBuildRuleCatchesNot
          "DL3013"
          "RUN pip install git+https://github.com/rtfd/r-ext.git@0.6-alpha#egg=r-ext"
      it "pip install unversioned git" $ do
        ruleCatches
          "DL3013"
          "RUN pip install git+https://github.com/rtfd/read-ext.git#egg=read-ext"
        onBuildRuleCatches
          "DL3013"
          "RUN pip install git+https://github.com/rtfd/read-ext.git#egg=read-ext"
      it "pip install upper bound" $ do
        ruleCatchesNot "DL3013" "RUN pip install 'alabaster>=0.7'"
        onBuildRuleCatchesNot "DL3013" "RUN pip install 'alabaster>=0.7'"
      it "pip install lower bound" $ do
        ruleCatchesNot "DL3013" "RUN pip install 'alabaster<0.7'"
        onBuildRuleCatchesNot "DL3013" "RUN pip install 'alabaster<0.7'"
      it "pip install excluded version" $ do
        ruleCatchesNot "DL3013" "RUN pip install 'alabaster!=0.7'"
        onBuildRuleCatchesNot "DL3013" "RUN pip install 'alabaster!=0.7'"
      it "pip install user directory" $ do
        ruleCatchesNot "DL3013" "RUN pip install MySQL_python==1.2.2 --user"
        onBuildRuleCatchesNot "DL3013" "RUN pip install MySQL_python==1.2.2 --user"
      it "pip install no pip version check" $ do
        ruleCatchesNot
          "DL3013"
          "RUN pip install MySQL_python==1.2.2 --disable-pip-version-check"
        onBuildRuleCatchesNot
          "DL3013"
          "RUN pip install MySQL_python==1.2.2 --disable-pip-version-check"
      it "pip install --index-url" $ do
        ruleCatchesNot
          "DL3013"
          "RUN pip install --index-url https://eg.com/foo foobar==1.0.0"
        onBuildRuleCatchesNot
          "DL3013"
          "RUN pip install --index-url https://eg.com/foo foobar==1.0.0"
      it "pip install index-url with -i flag" $ do
        ruleCatchesNot
          "DL3013"
          "RUN pip install --index-url https://eg.com/foo foobar==1.0.0"
        onBuildRuleCatchesNot
          "DL3013"
          "RUN pip install --index-url https://eg.com/foo foobar==1.0.0"
      it "pip install --index-url with --extra-index-url" $ do
        ruleCatchesNot
          "DL3013"
          "RUN pip install --index-url https://eg.com/foo --extra-index-url https://ex-eg.io/foo foobar==1.0.0"
        onBuildRuleCatchesNot
          "DL3013"
          "RUN pip install --index-url https://eg.com/foo --extra-index-url https://ex-eg.io/foo foobar==1.0.0"
      it "pip install no cache dir" $ do
        ruleCatchesNot "DL3013" "RUN pip install MySQL_python==1.2.2 --no-cache-dir"
        onBuildRuleCatchesNot "DL3013" "RUN pip install MySQL_python==1.2.2 --no-cache-dir"
      it "pip install constraints file - long version argument" $ do
        ruleCatchesNot "DL3013" "RUN pip install pykafka --constraint http://foo.bar.baz"
        onBuildRuleCatchesNot "DL3013" "RUN pip install pykafka --constraint http://foo.bar.baz"
      it "pip install constraints file - short version argument" $ do
        ruleCatchesNot "DL3013" "RUN pip install pykafka -c http://foo.bar.baz"
        onBuildRuleCatchesNot "DL3013" "RUN pip install pykafka -c http://foo.bar.baz"
      it "pip install --index-url with --extra-index-url with basic auth" $ do
        ruleCatchesNot
          "DL3013"
          "RUN pip install --index-url https://user:pass@eg.com/foo --extra-index-url https://user:pass@ex-eg.io/foo foobar==1.0.0"
        onBuildRuleCatchesNot
          "DL3013"
          "RUN pip install --index-url https://user:pass@eg.com/foo --extra-index-url https://user:pass@ex-eg.io/foo foobar==1.0.0"

    --
    describe "pip cache dir" $ do
      it "pip2 --no-cache-dir not used" $ do
        ruleCatches "DL3042" "RUN pip2 install MySQL_python"
        onBuildRuleCatches "DL3042" "RUN pip2 install MySQL_python"
      it "pip3 --no-cache-dir not used" $ do
        ruleCatches "DL3042" "RUN pip3 install MySQL_python"
        onBuildRuleCatches "DL3042" "RUN pip3 install MySQL_python"
      it "pip --no-cache-dir not used" $ do
        ruleCatches "DL3042" "RUN pip install MySQL_python"
        onBuildRuleCatches "DL3042" "RUN pip install MySQL_python"
      it "pip2 --no-cache-dir used" $ do
        ruleCatchesNot "DL3042" "RUN pip2 install MySQL_python --no-cache-dir"
        onBuildRuleCatchesNot "DL3042" "RUN pip2 install MySQL_python --no-cache-dir"
      it "pip3 --no-cache-dir used" $ do
        ruleCatchesNot "DL3042" "RUN pip3 install --no-cache-dir MySQL_python"
        onBuildRuleCatchesNot "DL3042" "RUN pip3 install --no-cache-dir MySQL_python"
      it "pip --no-cache-dir used" $ do
        ruleCatchesNot "DL3042" "RUN pip install MySQL_python --no-cache-dir"
        onBuildRuleCatchesNot "DL3042" "RUN pip install MySQL_python --no-cache-dir"
      it "don't match on pipx" $ do
        ruleCatchesNot "DL3042" "RUN pipx install software"
        onBuildRuleCatchesNot "DL3042" "Run pipx install software"
      it "don't match on pipenv" $ do
        ruleCatchesNot "DL3042" "RUN pipenv install library"
        onBuildRuleCatchesNot "DL3042" "RUN pipenv install library"
    --
    describe "npm pinning" $ do
      it "version pinned in package.json" $ do
        ruleCatchesNot "DL3016" "RUN npm install"
        onBuildRuleCatchesNot "DL3016" "RUN npm install"
      it "version pinned in package.json with arguments" $ do
        ruleCatchesNot "DL3016" "RUN npm install --progress=false"
        onBuildRuleCatchesNot "DL3016" "RUN npm install --progress=false"
      it "version pinned" $ do
        ruleCatchesNot "DL3016" "RUN npm install express@4.1.1"
        onBuildRuleCatchesNot "DL3016" "RUN npm install express@4.1.1"
      it "version pinned with scope" $ do
        ruleCatchesNot "DL3016" "RUN npm install @myorg/privatepackage@\">=0.1.0\""
        onBuildRuleCatchesNot "DL3016" "RUN npm install @myorg/privatepackage@\">=0.1.0\""
      it "version pinned multiple packages" $ do
        ruleCatchesNot "DL3016" "RUN npm install express@\"4.1.1\" sax@0.1.1"
        onBuildRuleCatchesNot "DL3016" "RUN npm install express@\"4.1.1\" sax@0.1.1"
      it "version pinned with --global" $ do
        ruleCatchesNot "DL3016" "RUN npm install --global express@\"4.1.1\""
        onBuildRuleCatchesNot "DL3016" "RUN npm install --global express@\"4.1.1\""
      it "version pinned with -g" $ do
        ruleCatchesNot "DL3016" "RUN npm install -g express@\"4.1.1\""
        onBuildRuleCatchesNot "DL3016" "RUN npm install -g express@\"4.1.1\""
      it "version does not have to be pinned for tarball suffix .tar" $ do
        ruleCatchesNot "DL3016" "RUN npm install package-v1.2.3.tar"
        onBuildRuleCatchesNot "DL3016" "RUN npm install package-v1.2.3.tar"
      it "version does not have to be pinned for tarball suffix .tar.gz" $ do
        ruleCatchesNot "DL3016" "RUN npm install package-v1.2.3.tar.gz"
        onBuildRuleCatchesNot "DL3016" "RUN npm install package-v1.2.3.tar.gz"
      it "version does not have to be pinned for tarball suffix .tgz" $ do
        ruleCatchesNot "DL3016" "RUN npm install package-v1.2.3.tgz"
        onBuildRuleCatchesNot "DL3016" "RUN npm install package-v1.2.3.tgz"
      it "version does not have to be pinned for folder - absolute path" $ do
        ruleCatchesNot "DL3016" "RUN npm install /folder"
        onBuildRuleCatchesNot "DL3016" "RUN npm install /folder"
      it "version does not have to be pinned for folder - relative path from current folder" $ do
        ruleCatchesNot "DL3016" "RUN npm install ./folder"
        onBuildRuleCatchesNot "DL3016" "RUN npm install ./folder"
      it "version does not have to be pinned for folder - relative path to parent folder" $ do
        ruleCatchesNot "DL3016" "RUN npm install ../folder"
        onBuildRuleCatchesNot "DL3016" "RUN npm install ../folder"
      it "version does not have to be pinned for folder - relative path from home" $ do
        ruleCatchesNot "DL3016" "RUN npm install ~/folder"
        onBuildRuleCatchesNot "DL3016" "RUN npm install ~/folder"
      it "commit pinned for git+ssh" $ do
        ruleCatchesNot
          "DL3016"
          "RUN npm install git+ssh://git@github.com:npm/npm.git#v1.0.27"
        onBuildRuleCatchesNot
          "DL3016"
          "RUN npm install git+ssh://git@github.com:npm/npm.git#v1.0.27"
      it "commit pinned for git+http" $ do
        ruleCatchesNot
          "DL3016"
          "RUN npm install git+http://isaacs@github.com/npm/npm#semver:^5.0"
        onBuildRuleCatchesNot
          "DL3016"
          "RUN npm install git+http://isaacs@github.com/npm/npm#semver:^5.0"
      it "commit pinned for git+https" $ do
        ruleCatchesNot
          "DL3016"
          "RUN npm install git+https://isaacs@github.com/npm/npm.git#v1.0.27"
        onBuildRuleCatchesNot
          "DL3016"
          "RUN npm install git+https://isaacs@github.com/npm/npm.git#v1.0.27"
      it "commit pinned for git" $ do
        ruleCatchesNot
          "DL3016"
          "RUN npm install git://github.com/npm/npm.git#v1.0.27"
        onBuildRuleCatchesNot
          "DL3016"
          "RUN npm install git://github.com/npm/npm.git#v1.0.27"
      it "npm run install is fine" $ do
        ruleCatchesNot
          "DL3016"
          "RUN npm run --crazy install"
        onBuildRuleCatchesNot
          "DL3016"
          "RUN npm run --crazy install"

      --version range is not supported
      it "version pinned with scope" $ do
        ruleCatchesNot "DL3016" "RUN npm install @myorg/privatepackage@\">=0.1.0 <0.2.0\""
        onBuildRuleCatchesNot "DL3016" "RUN npm install @myorg/privatepackage@\">=0.1.0 <0.2.0\""
      it "version not pinned" $ do
        ruleCatches "DL3016" "RUN npm install express"
        onBuildRuleCatches "DL3016" "RUN npm install express"
      it "version not pinned with scope" $ do
        ruleCatches "DL3016" "RUN npm install @myorg/privatepackage"
        onBuildRuleCatches "DL3016" "RUN npm install @myorg/privatepackage"
      it "version not pinned multiple packages" $ do
        ruleCatches "DL3016" "RUN npm install express sax@0.1.1"
        onBuildRuleCatches "DL3016" "RUN npm install express sax@0.1.1"
      it "version not pinned with --global" $ do
        ruleCatches "DL3016" "RUN npm install --global express"
        onBuildRuleCatches "DL3016" "RUN npm install --global express"
      it "commit not pinned for git+ssh" $ do
        ruleCatches "DL3016" "RUN npm install git+ssh://git@github.com:npm/npm.git"
        onBuildRuleCatches "DL3016" "RUN npm install git+ssh://git@github.com:npm/npm.git"
      it "commit not pinned for git+http" $ do
        ruleCatches "DL3016" "RUN npm install git+http://isaacs@github.com/npm/npm"
        onBuildRuleCatches "DL3016" "RUN npm install git+http://isaacs@github.com/npm/npm"
      it "commit not pinned for git+https" $ do
        ruleCatches
          "DL3016"
          "RUN npm install git+https://isaacs@github.com/npm/npm.git"
        onBuildRuleCatches
          "DL3016"
          "RUN npm install git+https://isaacs@github.com/npm/npm.git"
      it "commit not pinned for git" $ do
        ruleCatches "DL3016" "RUN npm install git://github.com/npm/npm.git"
        onBuildRuleCatches "DL3016" "RUN npm install git://github.com/npm/npm.git"
    --
    describe "use SHELL" $ do
      it "RUN ln" $ do
        ruleCatches "DL4005" "RUN ln -sfv /bin/bash /bin/sh"
        onBuildRuleCatches "DL4005" "RUN ln -sfv /bin/bash /bin/sh"
      it "RUN ln with unrelated symlinks" $ do
        ruleCatchesNot "DL4005" "RUN ln -sf /bin/true /sbin/initctl"
        onBuildRuleCatchesNot "DL4005" "RUN ln -sf /bin/true /sbin/initctl"
      it "RUN ln with multiple acceptable commands" $ do
        ruleCatchesNot "DL4005" "RUN ln -s foo bar && unrelated && something_with /bin/sh"
        onBuildRuleCatchesNot "DL4005" "RUN ln -s foo bar && unrelated && something_with /bin/sh"
    --
    --
    describe "Shellcheck" $ do
      it "runs shellchek on RUN instructions" $ do
        assertChecks "RUN echo $MISSING_QUOTES" failsShellcheck
        assertOnBuildChecks "RUN echo $MISSING_QUOTES" failsShellcheck
      it "not warns on valid scripts" $ do
        assertChecks "RUN echo foo" passesShellcheck
        assertOnBuildChecks "RUN echo foo" passesShellcheck

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
              assertChecks dockerFile passesShellcheck
              assertOnBuildChecks dockerFile passesShellcheck

      it "Complain on missing env vars" $
        let dockerFile =
              Text.unlines
                [ "RUN echo \"$RTTP_PROXY\""
                ]
         in do
              assertChecks dockerFile failsShellcheck
              assertOnBuildChecks dockerFile failsShellcheck

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
              assertChecks dockerFile passesShellcheck
              assertOnBuildChecks dockerFile failsShellcheck

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
              assertChecks dockerFile failsShellcheck
              assertOnBuildChecks dockerFile failsShellcheck

      it "Defaults the shell to sh" $
        let dockerFile =
              Text.unlines
                [ "RUN echo $RANDOM"
                ]
         in do
              assertChecks dockerFile failsShellcheck
              assertOnBuildChecks dockerFile failsShellcheck

      it "Can change the shell check to bash" $
        let dockerFile =
              Text.unlines
                [ "SHELL [\"/bin/bash\", \"-eo\", \"pipefail\", \"-c\"]",
                  "RUN echo $RANDOM"
                ]
         in do
              assertChecks dockerFile passesShellcheck
              -- This is debatable, as it should actaully pass, but detecting it correctly
              -- is quite difficult
              assertOnBuildChecks dockerFile failsShellcheck
      it "Resets the SHELL to sh after a FROM" $
        let dockerFile =
              Text.unlines
                [ "SHELL [\"/bin/bash\", \"-eo\", \"pipefail\", \"-c\"]",
                  "FROM debian",
                  "RUN echo $RANDOM"
                ]
         in do
              assertChecks dockerFile failsShellcheck
              assertOnBuildChecks dockerFile failsShellcheck

      it "Does not complain on ash shell" $
        let dockerFile =
              Text.unlines
                [ "SHELL [\"/bin/ash\", \"-o\", \"pipefail\", \"-c\"]",
                  "RUN echo hello"
                ]
         in do
              assertChecks dockerFile passesShellcheck
              assertOnBuildChecks dockerFile passesShellcheck

      it "Does not complain on powershell" $
        let dockerFile =
              Text.unlines
                [ "SHELL [\"pwsh\", \"-c\"]",
                  "RUN Get-Variable PSVersionTable | Select-Object -ExpandProperty Value"
                ]
         in do
              assertChecks dockerFile passesShellcheck
              assertOnBuildChecks dockerFile passesShellcheck
    --
    describe "COPY rules" $ do
      it "use add" $ ruleCatches "DL3010" "COPY packaged-app.tar /usr/src/app"
      it "use not add" $ ruleCatchesNot "DL3010" "COPY package.json /usr/src/app"
    --
    describe "other rules" $ do
      it "apt-get auto yes" $ do
        ruleCatches "DL3014" "RUN apt-get install python"
        onBuildRuleCatches "DL3014" "RUN apt-get install python"
      it "apt-get yes shortflag" $ do
        ruleCatchesNot "DL3014" "RUN apt-get install -yq python"
        onBuildRuleCatchesNot "DL3014" "RUN apt-get install -yq python"
      it "apt-get yes quiet level 2 implies -y" $ do
        ruleCatchesNot "DL3014" "RUN apt-get install -qq python"
        onBuildRuleCatchesNot "DL3014" "RUN apt-get install -qq python"
      it "apt-get yes different pos" $ do
        ruleCatchesNot "DL3014" "RUN apt-get install -y python"
        onBuildRuleCatchesNot "DL3014" "RUN apt-get install -y python"
      it "apt-get with auto yes" $ do
        ruleCatchesNot "DL3014" "RUN apt-get -y install python"
        onBuildRuleCatchesNot "DL3014" "RUN apt-get -y install python"
      it "apt-get with auto expanded yes" $ do
        ruleCatchesNot "DL3014" "RUN apt-get --yes install python"
        onBuildRuleCatchesNot "DL3014" "RUN apt-get --yes install python"
      it "apt-get with assume-yes" $ do
        ruleCatchesNot "DL3014" "RUN apt-get --assume-yes install python"
        onBuildRuleCatchesNot "DL3014" "RUN apt-get --assume-yes install python"
      it "apt-get install recommends" $ do
        ruleCatchesNot
          "DL3015"
          "RUN apt-get install --no-install-recommends python"
        onBuildRuleCatchesNot
          "DL3015"
          "RUN apt-get install --no-install-recommends python"
      it "apt-get no install recommends" $ do
        ruleCatches "DL3015" "RUN apt-get install python"
        onBuildRuleCatches "DL3015" "RUN apt-get install python"
      it "apt-get no install recommends" $ do
        ruleCatches "DL3015" "RUN apt-get -y install python"
        onBuildRuleCatches "DL3015" "RUN apt-get -y install python"
      it "apt-get no install recommends via option" $ do
        ruleCatchesNot "DL3015" "RUN apt-get -o APT::Install-Recommends=false install python"
        onBuildRuleCatchesNot "DL3015" "RUN apt-get -o APT::Install-Recommends=false install python"
      it "apt-get version" $ do
        ruleCatchesNot "DL3008" "RUN apt-get install -y python=1.2.2"
        onBuildRuleCatchesNot "DL3008" "RUN apt-get install -y python=1.2.2"
      it "apt-get version" $ do
        ruleCatchesNot "DL3008" "RUN apt-get install ./wkhtmltox_0.12.5-1.bionic_amd64.deb"
        onBuildRuleCatchesNot "DL3008" "RUN apt-get install ./wkhtmltox_0.12.5-1.bionic_amd64.deb"
      it "apt-get pinned" $ do
        ruleCatchesNot
          "DL3008"
          "RUN apt-get -y --no-install-recommends install nodejs=0.10"
        onBuildRuleCatchesNot
          "DL3008"
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
              ruleCatchesNot "DL3008" $ Text.unlines dockerFile
              onBuildRuleCatchesNot "DL3008" $ Text.unlines dockerFile

      it "has maintainer" $ ruleCatches "DL4000" "FROM debian\nMAINTAINER Lukas"
      it "has maintainer first" $ ruleCatches "DL4000" "MAINTAINER Lukas\nFROM DEBIAN"
      it "has no maintainer" $ ruleCatchesNot "DL4000" "FROM debian"
      it "using add" $ ruleCatches "DL3020" "ADD file /usr/src/app/"

      it "many cmds" $
        let dockerFile =
              [ "FROM debian",
                "CMD bash",
                "RUN foo",
                "CMD another"
              ]
         in ruleCatches "DL4003" $ Text.unlines dockerFile

      it "single cmds, different stages" $
        let dockerFile =
              [ "FROM debian as distro1",
                "CMD bash",
                "RUN foo",
                "FROM debian as distro2",
                "CMD another"
              ]
         in ruleCatchesNot "DL4003" $ Text.unlines dockerFile

      it "many cmds, different stages" $
        let dockerFile =
              [ "FROM debian as distro1",
                "CMD bash",
                "RUN foo",
                "CMD another",
                "FROM debian as distro2",
                "CMD another"
              ]
         in ruleCatches "DL4003" $ Text.unlines dockerFile

      it "single cmd" $ ruleCatchesNot "DL4003" "CMD /bin/true"
      it "no cmd" $ ruleCatchesNot "DL4004" "FROM busybox"

      it "many entrypoints" $
        let dockerFile =
              [ "FROM debian",
                "ENTRYPOINT bash",
                "RUN foo",
                "ENTRYPOINT another"
              ]
         in ruleCatches "DL4004" $ Text.unlines dockerFile

      it "single entrypoint, different stages" $
        let dockerFile =
              [ "FROM debian as distro1",
                "ENTRYPOINT bash",
                "RUN foo",
                "FROM debian as distro2",
                "ENTRYPOINT another"
              ]
         in ruleCatchesNot "DL4004" $ Text.unlines dockerFile

      it "many entrypoints, different stages" $
        let dockerFile =
              [ "FROM debian as distro1",
                "ENTRYPOINT bash",
                "RUN foo",
                "ENTRYPOINT another",
                "FROM debian as distro2",
                "ENTRYPOINT another"
              ]
         in ruleCatches "DL4004" $ Text.unlines dockerFile

      it "single entry" $ ruleCatchesNot "DL4004" "ENTRYPOINT /bin/true"
      it "no entry" $ ruleCatchesNot "DL4004" "FROM busybox"
      it "workdir relative" $ ruleCatches "DL3000" "WORKDIR relative/dir"
      it "workdir absolute" $ ruleCatchesNot "DL3000" "WORKDIR /usr/local"
      it "workdir variable" $ ruleCatchesNot "DL3000" "WORKDIR ${work}"
      it "workdir relative single quotes" $ ruleCatches "DL3000" "WORKDIR \'relative/dir\'"
      it "workdir absolute single quotes" $ ruleCatchesNot "DL3000" "WORKDIR \'/usr/local\'"
      -- no test for variable/single quotes since the variable would not expand.
      it "workdir relative double quotes" $ ruleCatches "DL3000" "WORKDIR \"relative/dir\""
      it "workdir absolute double quotes" $ ruleCatchesNot "DL3000" "WORKDIR \"/usr/local\""
      it "workdir variable double quotes" $ ruleCatchesNot "DL3000" "WORKDIR \"${dir}\""
      it "scratch" $ ruleCatchesNot "DL3006" "FROM scratch"
    --
    describe "add files and archives" $ do
      it "add for tar" $ ruleCatchesNot "DL3020" "ADD file.tar /usr/src/app/"
      it "add for zip" $ ruleCatchesNot "DL3020" "ADD file.zip /usr/src/app/"
      it "add for gzip" $ ruleCatchesNot "DL3020" "ADD file.gz /usr/src/app/"
      it "add for bz2" $ ruleCatchesNot "DL3020" "ADD file.bz2 /usr/src/app/"
      it "add for xz" $ ruleCatchesNot "DL3020" "ADD file.xz /usr/src/app/"
      it "add for tgz" $ ruleCatchesNot "DL3020" "ADD file.tgz /usr/src/app/"
      it "add for url" $ ruleCatchesNot "DL3020" "ADD http://file.com /usr/src/app/"
    --
    describe "copy last argument" $ do
      it "no warn on 2 args" $ ruleCatchesNot "DL3021" "COPY foo bar"
      it "warn on 3 args" $ ruleCatches "DL3021" "COPY foo bar baz"
      it "no warn on 3 args" $ ruleCatchesNot "DL3021" "COPY foo bar baz/"
    --
    describe "copy from existing alias" $ do
      it "warn on missing alias" $ ruleCatches "DL3022" "COPY --from=foo bar ."
      it "warn on alias defined after" $
        let dockerFile =
              [ "FROM scratch",
                "COPY --from=build foo .",
                "FROM node as build",
                "RUN baz"
              ]
         in ruleCatches "DL3022" $ Text.unlines dockerFile
      it "don't warn on correctly defined aliases" $
        let dockerFile =
              [ "FROM scratch as build",
                "RUN foo",
                "FROM node",
                "COPY --from=build foo .",
                "RUN baz"
              ]
         in ruleCatchesNot "DL3022" $ Text.unlines dockerFile
    --
    describe "copy from own FROM" $ do
      it "warn on copying from your the same FROM" $
        let dockerFile =
              [ "FROM node as foo",
                "COPY --from=foo bar ."
              ]
         in ruleCatches "DL3023" $ Text.unlines dockerFile
      it "don't warn on copying from other sources" $
        let dockerFile =
              [ "FROM scratch as build",
                "RUN foo",
                "FROM node as run",
                "COPY --from=build foo .",
                "RUN baz"
              ]
         in ruleCatchesNot "DL3023" $ Text.unlines dockerFile
    --
    describe "Duplicate aliases" $ do
      it "warn on duplicate aliases" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN something",
                "FROM scratch as foo",
                "RUN something"
              ]
         in ruleCatches "DL3024" $ Text.unlines dockerFile
      it "don't warn on unique aliases" $
        let dockerFile =
              [ "FROM scratch as build",
                "RUN foo",
                "FROM node as run",
                "RUN baz"
              ]
         in ruleCatchesNot "DL3024" $ Text.unlines dockerFile
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
         in ruleCatchesNot "DL3002" $ Text.unlines dockerFile
      it "ignores only the given rule" $
        let dockerFile =
              [ "FROM scratch",
                "# hadolint ignore=DL3001",
                "USER root"
              ]
         in ruleCatches "DL3002" $ Text.unlines dockerFile
      it "ignores only the given rule, when multiple passed" $
        let dockerFile =
              [ "FROM scratch",
                "# hadolint ignore=DL3001,DL3002",
                "USER root"
              ]
         in ruleCatchesNot "DL3002" $ Text.unlines dockerFile
      it "ignores the rule only if directly above the instruction" $
        let dockerFile =
              [ "# hadolint ignore=DL3001,DL3002",
                "FROM ubuntu",
                "USER root"
              ]
         in ruleCatches "DL3002" $ Text.unlines dockerFile
      it "won't ignore the rule if passed invalid rule names" $
        let dockerFile =
              [ "FROM scratch",
                "# hadolint ignore=crazy,DL3002",
                "USER root"
              ]
         in ruleCatches "DL3002" $ Text.unlines dockerFile
      it "ignores multiple rules correctly, even with some extra whitespace" $
        let dockerFile =
              [ "FROM node as foo",
                "# hadolint ignore=DL3023, DL3021",
                "COPY --from=foo bar baz ."
              ]
         in do
              ruleCatchesNot "DL3023" $ Text.unlines dockerFile
              ruleCatchesNot "DL3021" $ Text.unlines dockerFile
    --
    describe "JSON notation in ENTRYPOINT and CMD" $ do
      it "warn on ENTRYPOINT" $
        let dockerFile =
              [ "FROM node as foo",
                "ENTRYPOINT something"
              ]
         in ruleCatches "DL3025" $ Text.unlines dockerFile
      it "don't warn on ENTRYPOINT json notation" $
        let dockerFile =
              [ "FROM scratch as build",
                "ENTRYPOINT [\"foo\", \"bar\"]"
              ]
         in ruleCatchesNot "DL3025" $ Text.unlines dockerFile
      it "warn on CMD" $
        let dockerFile =
              [ "FROM node as foo",
                "CMD something"
              ]
         in ruleCatches "DL3025" $ Text.unlines dockerFile
      it "don't warn on CMD json notation" $
        let dockerFile =
              [ "FROM scratch as build",
                "CMD [\"foo\", \"bar\"]",
                "CMD [ \"foo\", \"bar\" ]"
              ]
         in ruleCatchesNot "DL3025" $ Text.unlines dockerFile

    --
    describe "Detects missing pipefail option" $ do
      it "warn on missing pipefail" $
        let dockerFile =
              [ "FROM scratch",
                "RUN wget -O - https://some.site | wc -l > /number"
              ]
         in ruleCatches "DL4006" $ Text.unlines dockerFile
      it "don't warn on commands with no pipes" $
        let dockerFile =
              [ "FROM scratch as build",
                "RUN wget -O - https://some.site && wc -l file > /number"
              ]
         in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
      it "don't warn on commands with pipes and the pipefail option" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"/bin/bash\", \"-eo\", \"pipefail\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number"
              ]
         in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
      it "don't warn on commands with pipes and the pipefail option 2" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"/bin/bash\", \"-e\", \"-o\", \"pipefail\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number"
              ]
         in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
      it "don't warn on commands with pipes and the pipefail option 3" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"/bin/bash\", \"-o\", \"errexit\", \"-o\", \"pipefail\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number"
              ]
         in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
      it "don't warn on commands with pipes and the pipefail zsh" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"/bin/zsh\", \"-o\", \"pipefail\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number"
              ]
         in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
      it "don't warn on powershell" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"pwsh\", \"-c\"]",
                "RUN Get-Variable PSVersionTable | Select-Object -ExpandProperty Value"
              ]
         in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
      it "warns when using plain sh" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"/bin/sh\", \"-o\", \"pipefail\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number"
              ]
         in ruleCatches "DL4006" $ Text.unlines dockerFile
      it "warn on missing pipefail in the next image" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"/bin/bash\", \"-o\", \"pipefail\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number",
                "FROM scratch as build2",
                "RUN wget -O - https://some.site | wc -l file > /number"
              ]
         in ruleCatches "DL4006" $ Text.unlines dockerFile
      it "warn on missing pipefail if next SHELL is not using it" $
        let dockerFile =
              [ "FROM scratch as build",
                "SHELL [\"/bin/bash\", \"-o\", \"pipefail\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number",
                "SHELL [\"/bin/sh\", \"-c\"]",
                "RUN wget -O - https://some.site | wc -l file > /number"
              ]
         in ruleCatches "DL4006" $ Text.unlines dockerFile
    --
    describe "Allowed docker registries" $ do
      it "does not warn on empty allowed registries" $ do
        let dockerFile =
              [ "FROM random.com/debian"
              ]
        ruleCatchesNot "DL3026" $ Text.unlines dockerFile

      it "warn on non-allowed registry" $ do
        let dockerFile =
              [ "FROM random.com/debian"
              ]
        let ?rulesConfig = Hadolint.Process.RulesConfig ["docker.io"] Map.empty False
        ruleCatches "DL3026" $ Text.unlines dockerFile

      it "does not warn on allowed registries" $ do
        let dockerFile =
              [ "FROM random.com/debian"
              ]
        let ?rulesConfig = Hadolint.Process.RulesConfig ["x.com", "random.com"] Map.empty False
        ruleCatchesNot "DL3026" $ Text.unlines dockerFile

      it "doesn't warn on scratch image" $ do
        let dockerFile =
              [ "FROM scratch"
              ]
        let ?rulesConfig = Hadolint.Process.RulesConfig ["x.com", "random.com"] Map.empty False
        ruleCatchesNot "DL3026" $ Text.unlines dockerFile

      it "allows boths all forms of docker.io" $ do
        let dockerFile =
              [ "FROM ubuntu:18.04 AS builder1",
                "FROM zemanlx/ubuntu:18.04 AS builder2",
                "FROM docker.io/zemanlx/ubuntu:18.04 AS builder3"
              ]
        let ?rulesConfig = Hadolint.Process.RulesConfig ["docker.io"] Map.empty False
        ruleCatchesNot "DL3026" $ Text.unlines dockerFile

      it "allows using previous stages" $ do
        let dockerFile =
              [ "FROM random.com/foo AS builder1",
                "FROM builder1 AS builder2"
              ]
        let ?rulesConfig = Hadolint.Process.RulesConfig ["random.com"] Map.empty False
        ruleCatchesNot "DL3026" $ Text.unlines dockerFile
    --
    describe "Wget or Curl" $ do
      it "warns when using both wget and curl" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz",
                "RUN curl localhost"
              ]
         in ruleCatches "DL4001" $ Text.unlines dockerFile
      it "warns when using both wget and curl in same instruction" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz && curl localhost"
              ]
         in ruleCatches "DL4001" $ Text.unlines dockerFile
      it "does not warn when using only wget" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz"
              ]
         in ruleCatchesNot "DL4001" $ Text.unlines dockerFile
      it "does not warn when using both curl and wget in different stages" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz",
                "FROM scratch",
                "RUN curl localhost"
              ]
         in ruleCatchesNot "DL4001" $ Text.unlines dockerFile
      it "does not warns when using both, on a single stage" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz",
                "RUN curl localhost",
                "FROM scratch",
                "RUN curl localhost"
              ]
         in ruleCatches "DL4001" $ Text.unlines dockerFile
      it "only warns on the relevant RUN instruction" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz",
                "RUN curl my.xyz",
                "RUN echo hello"
              ]
         in assertChecks
              (Text.unlines dockerFile)
              (failsWith 1 "DL4001")

      it "only warns on many relevant RUN instructions" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz",
                "RUN curl my.xyz",
                "RUN echo hello",
                "RUN wget foo.com"
              ]
         in assertChecks
              (Text.unlines dockerFile)
              (failsWith 2 "DL4001")
    --
    describe "Wget no progress" $ do
      it "warns when using wget without --progress option" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget my.xyz"
              ]
         in ruleCatches "DL3047" $ Text.unlines dockerFile
      it "does not warn when running with --progress option" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget --progress=dot:giga my.xyz"
              ]
         in ruleCatchesNot "DL3047" $ Text.unlines dockerFile
      it "does not warn when running with -q (quiet short option) and without --progress option" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget -q my.xyz"
              ]
         in ruleCatchesNot "DL3047" $ Text.unlines dockerFile
      it "does not warn when running with --quiet (quiet long option) and without --progress option" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget --quiet my.xyz"
              ]
         in ruleCatchesNot "DL3047" $ Text.unlines dockerFile
      it "does not warn when running with -nv (no-verbose short option) and without --progress option" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget -nv my.xyz"
              ]
         in ruleCatchesNot "DL3047" $ Text.unlines dockerFile
      it "does not warn when running with --no-verbose (no-verbose long option) and without --progress option" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget --no-verbose my.xyz"
              ]
         in ruleCatchesNot "DL3047" $ Text.unlines dockerFile
      it "does not warn when running with --output-file (output-file long option) and without --progress option" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget --output-file=/tmp/wget.log my.xyz"
              ]
         in ruleCatchesNot "DL3047" $ Text.unlines dockerFile
      it "does not warn when running with -o (output-file long option) and without --progress option" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget -o /tmp/wget.log my.xyz"
              ]
         in ruleCatchesNot "DL3047" $ Text.unlines dockerFile
      it "does not warn when running with --append-output (append-output long option) and without --progress option" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget --append-output=/tmp/wget.log my.xyz"
              ]
         in ruleCatchesNot "DL3047" $ Text.unlines dockerFile
      it "does not warn when running with -a (append-output long option) and without --progress option" $
        let dockerFile =
              [ "FROM node as foo",
                "RUN wget -a /tmp/wget.log my.xyz"
              ]
         in ruleCatchesNot "DL3047" $ Text.unlines dockerFile
    --
    describe "DL3012 - Multiple `HEALTHCHECK` instructions" $ do
      it "ok with no HEALTHCHECK instruction" $
        ruleCatchesNot "DL3012" "FROM scratch"
      it "ok with one HEALTHCHECK instruction" $
        ruleCatchesNot "DL3012" "FROM scratch\nHEALTHCHECK CMD /bin/bla"
      it "ok with two HEALTHCHECK instructions in two stages" $
        ruleCatchesNot "DL3012" "FROM scratch\nHEALTHCHECK CMD /bin/bla1\nFROM scratch\nHEALTHCHECK CMD /bin/bla2"
      it "warn with two HEALTHCHECK instructions" $
        ruleCatches "DL3012" "FROM scratch\nHEALTHCHECK CMD /bin/bla1\nHEALTHCHECK CMD /bin/bla2"
    --
    describe "DL3057 - `HEALTHCHECK instruction missing" $ do
      it "warn with no HEALTHCHECK instructions" $
        ruleCatches "DL3057" "FROM scratch"
      it "ok with one HEALTHCHECK instruction" $
        ruleCatchesNot "DL3057" "FROM scratch\nHEALTHCHECK CMD /bin/bla"
      it "ok with inheriting HEALTHCHECK instruction" $
        ruleCatchesNot "DL3057" "FROM scratch AS base\nHEALTHCHECK CMD /bin/bla\nFROM base"
      it "warn when not inheriting with no HEALTHCHECK instruction" $
        ruleCatches "DL3057" "FROM scratch AS base\nHEALTHCHECK CMD /bin/bla\nFROM scratch"
    --
    describe "ONBUILD" $ do
      it "error when using `ONBUILD` within `ONBUILD`" $
        let dockerFile =
              [ "ONBUILD ONBUILD RUN anything"
              ]
         in ruleCatches "DL3043" $ Text.unlines dockerFile
      it "error when using `FROM` within `ONBUILD`" $
        let dockerFile =
              [ "ONBUILD FROM debian:buster"
              ]
         in ruleCatches "DL3043" $ Text.unlines dockerFile
      it "error when using `MAINTAINER` within `ONBUILD`" $
        let dockerFile =
              [ "ONBUILD MAINTAINER \"BoJack Horseman\""
              ]
         in ruleCatches "DL3043" $ Text.unlines dockerFile
      it "ok with `ADD`" $ ruleCatchesNot "DL3043" "ONBUILD ADD anything anywhere"
      it "ok with `USER`" $ ruleCatchesNot "DL3043" "ONBUILD USER anything"
      it "ok with `LABEL`" $ ruleCatchesNot "DL3043" "ONBUILD LABEL bla=\"blubb\""
      it "ok with `STOPSIGNAL`" $ ruleCatchesNot "DL3043" "ONBUILD STOPSIGNAL anything"
      it "ok with `COPY`" $ ruleCatchesNot "DL3043" "ONBUILD COPY anything anywhere"
      it "ok with `RUN`" $ ruleCatchesNot "DL3043" "ONBUILD RUN anything"
      it "ok with `CMD`" $ ruleCatchesNot "DL3043" "ONBUILD CMD anything"
      it "ok with `SHELL`" $ ruleCatchesNot "DL3043" "ONBUILD SHELL anything"
      it "ok with `WORKDIR`" $ ruleCatchesNot "DL3043" "ONBUILD WORKDIR anything"
      it "ok with `EXPOSE`" $ ruleCatchesNot "DL3043" "ONBUILD EXPOSE 69"
      it "ok with `VOLUME`" $ ruleCatchesNot "DL3043" "ONBUILD VOLUME anything"
      it "ok with `ENTRYPOINT`" $ ruleCatchesNot "DL3043" "ONBUILD ENTRYPOINT anything"
      it "ok with `ENV`" $ ruleCatchesNot "DL3043" "ONBUILD ENV MYVAR=\"bla\""
      it "ok with `ARG`" $ ruleCatchesNot "DL3043" "ONBUILD ARG anything"
      it "ok with `HEALTHCHECK`" $ ruleCatchesNot "DL3043" "ONBUILD HEALTHCHECK NONE"
      it "ok with `FROM` outside of `ONBUILD`" $ ruleCatchesNot "DL3043" "FROM debian:buster"
      it "ok with `MAINTAINER` outside of `ONBUILD`" $ ruleCatchesNot "DL3043" "MAINTAINER \"Some Guy\""
    --
    describe "Selfreferencing `ENV`s" $ do
      it "ok with normal ENV" $
        ruleCatchesNot "DL3044" "ENV BLA=\"blubb\"\nENV BLUBB=\"${BLA}/blubb\""
      it "ok with partial match 1" $
        ruleCatchesNot "DL3044" "ENV BLA=\"blubb\" BLUBB=\"${FOOBLA}/blubb\""
      it "ok with partial match 2" $
        ruleCatchesNot "DL3044" "ENV BLA=\"blubb\" BLUBB=\"${BLAFOO}/blubb\""
      it "ok with partial match 3" $
        ruleCatchesNot "DL3044" "ENV BLA=\"blubb\" BLUBB=\"$FOOBLA/blubb\""
      it "ok with partial match 4" $
        ruleCatchesNot "DL3044" "ENV BLA=\"blubb\" BLUBB=\"$BLAFOO/blubb\""
      it "fail with partial match 5" $
        ruleCatches "DL3044" "ENV BLA=\"blubb\" BLUBB=\"$BLA/$BLAFOO/blubb\""
      it "ok with parial match 6" $
        ruleCatchesNot "DL3044" "ENV BLA=\"blubb\" BLUBB=\"BLA/$BLAFOO/BLA\""
      it "ok when previously defined in `ARG`" $
        ruleCatchesNot "DL3044" "ARG BLA\nENV BLA=${BLA}"
      it "ok when previously defined in `ENV`" $
        ruleCatchesNot "DL3044" "ENV BLA blubb\nENV BLA=${BLA}"
      it "ok with referencing a variable on its own right hand side" $
        ruleCatchesNot "DL3044" "ENV PATH=/bla:${PATH}"
      it "ok with referencing a variable on its own right side twice in different `ENV`s" $
        ruleCatchesNot "DL3044" "ENV PATH=/bla:${PATH}\nENV PATH=/blubb:${PATH}"
      it "fail when referencing a variable on its own right side twice within the same `ENV`" $
        ruleCatches "DL3044" "ENV PATH=/bla:${PATH} PATH=/blubb:${PATH}"
      it "fail with selfreferencing with curly braces ENV" $
        ruleCatches "DL3044" "ENV BLA=\"blubb\" BLUBB=\"${BLA}/blubb\""
      it "fail with selfreferencing without curly braces ENV" $
        ruleCatches "DL3044" "ENV BLA=\"blubb\" BLUBB=\"$BLA/blubb\""
      it "fail with full match 1" $
        ruleCatches "DL3044" "ENV BLA=\"blubb\" BLUBB=\"$BLA\""
      it "fail with full match 2" $
        ruleCatches "DL3044" "ENV BLA=\"blubb\" BLUBB=\"${BLA}\""
    --
    describe "warn when using `useradd` with long UID and without `-l`" $ do
      it "ok with `useradd` alone" $ ruleCatchesNot "DL3046" "RUN useradd luser"
      it "ok with `useradd` short uid" $ ruleCatchesNot "DL3046" "RUN useradd -u 12345 luser"
      it "ok with `useradd` long uid and flag `-l`" $ ruleCatchesNot "DL3046" "RUN useradd -l -u 123456 luser"
      it "ok with `useradd` and just flag `-l`" $ ruleCatchesNot "DL3046" "RUN useradd -l luser"
      it "warn when `useradd` and long uid without flag `-l`" $ ruleCatches "DL3046" "RUN useradd -u 123456 luser"
    --
    describe "Missing label rule tests" $
      let ?rulesConfig = Hadolint.Process.RulesConfig [] (Map.fromList [("foo", Rule.RawText)]) False
       in do
    -- single stage tests
      it "not ok: single stage, no label" $ do
        ruleCatches "DL3049" "FROM baseimage"
        onBuildRuleCatches "DL3049" "FROM baseimage"
      it "not ok: single stage, wrong label" $ do
        ruleCatches "DL3049" "FROM baseimage\nLABEL bar=\"baz\""
        onBuildRuleCatches "DL3049" "FROM baseimage\nLABEL bar=\"baz\""
      it "ok: single stage, label present" $ do
        ruleCatchesNot "DL3049" "FROM baseimage\nLABEL foo=\"bar\""
        onBuildRuleCatchesNot "DL3049" "FROM baseimage\nLABEL foo=\"bar\""
    -- multi stage tests
      it "warn twice: two stages, no labels" $
        let dockerFile =
              [ "FROM stage1",
                "FROM stage2"
              ]
         in assertChecks
              (Text.unlines dockerFile)
              (failsWith 2 "DL3049")
      it "warn twice: two stages, wrong labels only" $
        let dockerFile =
              [ "FROM stage1",
                "LABEL bar=\"baz\"",
                "FROM stage2",
                "LABEL buzz=\"fuzz\""
              ]
         in assertChecks
              (Text.unlines dockerFile)
              (failsWith 2 "DL3049")
      it "warn once: two stages, label present in second only" $
        let dockerFile =
              [ "FROM baseimage",
                "FROM newimage",
                "LABEL foo=\"bar\""
              ]
         in assertChecks
              (Text.unlines dockerFile)
              (failsWith 1 "DL3049")
      it "warn once: two stages, no inheritance, wrong label in one" $
        let dockerFile =
              [ "FROM baseimage",
                "LABEL baz=\"bar\"",
                "FROM newimage",
                "LABEL foo=\"bar\""
              ]
         in assertChecks
              (Text.unlines dockerFile)
              (failsWith 1 "DL3049")
      it "warn once: two stages, inheritance, label only defined in second stage" $
        let dockerFile =
              [ "FROM baseimage as base",
                "FROM base",
                "LABEL foo=\"bar\""
              ]
         in assertChecks
              (Text.unlines dockerFile)
              (failsWith 1 "DL3049")
      it "don't warn: two stages, inheritance" $
        let dockerFile =
              [ "FROM baseimage as base",
                "LABEL foo=\"bar\"",
                "FROM base"
              ]
         in assertChecks
              (Text.unlines dockerFile)
              (failsWith 0 "DL3049")
    describe "Strict Labels" $
      let ?rulesConfig = Hadolint.Process.RulesConfig [] (Map.fromList [("required", Rule.RawText)]) True
       in do
      it "ok with no label" $ do
        ruleCatchesNot "DL3050" ""
        onBuildRuleCatchesNot "DL3050" ""
      it "ok with required label" $ do
        ruleCatchesNot "DL3050" "LABEL required=\"foo\""
        onBuildRuleCatchesNot "DL3050" "LABEL required=\"bar\""
      it "not ok with just other label" $ do
        ruleCatches "DL3050" "LABEL other=\"bar\""
        onBuildRuleCatches "DL3050" "LABEL other=\"bar\""
      it "not ok with other label and required label" $ do
        ruleCatches "DL3050" "LABEL required=\"foo\" other=\"bar\""
        onBuildRuleCatches "DL3050" "LABEL required=\"foo\" other=\"bar\""
    describe "Label is not empty rule" $
      let ?rulesConfig = Hadolint.Process.RulesConfig [] (Map.fromList [("emptylabel", Rule.RawText)]) False
       in do
      it "not ok with label empty" $ do
        ruleCatches "DL3051" "LABEL emptylabel=\"\""
        onBuildRuleCatches "DL3051" "LABEL emptylabel=\"\""
      it "ok with label not empty" $ do
        ruleCatchesNot "DL3051" "LABEL emptylabel=\"foo\""
        onBuildRuleCatchesNot "DL3051" "LABEL emptylabel=\"bar\""
      it "ok with other label empty" $ do
        ruleCatchesNot "DL3051" "LABEL other=\"\""
        onBuildRuleCatchesNot "DL3051" "LABEL other=\"\""
    describe "Label is not URL rule" $
      let ?rulesConfig = Hadolint.Process.RulesConfig [] (Map.fromList [("urllabel", Rule.Url)]) False
       in do
      it "not ok with label not containing URL" $ do
        ruleCatches "DL3052" "LABEL urllabel=\"not-url\""
        onBuildRuleCatches "DL3052" "LABEL urllabel=\"not-url\""
      it "ok with label containing URL" $ do
        ruleCatchesNot "DL3052" "LABEL urllabel=\"http://example.com\""
        onBuildRuleCatchesNot "DL3052" "LABEL urllabel=\"http://example.com\""
      it "ok with other label not containing URL" $ do
        ruleCatchesNot "DL3052" "LABEL other=\"foo\""
        onBuildRuleCatchesNot "DL3052" "LABEL other=\"bar\""
    describe "Label is not RFC3339 date rule" $
      let ?rulesConfig = Hadolint.Process.RulesConfig [] (Map.fromList [("datelabel", Rule.Rfc3339)]) False
       in do
      it "not ok with label not containing RFC3339 date" $ do
        ruleCatches "DL3053" "LABEL datelabel=\"not-date\""
        onBuildRuleCatches "DL3053" "LABEL datelabel=\"not-date\""
      it "ok with label containing RFC3339 date" $ do
        ruleCatchesNot "DL3053" "LABEL datelabel=\"2021-03-10T10:26:33.564595127+01:00\""
        onBuildRuleCatchesNot "DL3053" "LABEL datelabel=\"2021-03-10T10:26:33.564595127+01:00\""
      it "ok with other label not containing RFC3339 date" $ do
        ruleCatchesNot "DL3053" "LABEL other=\"doo\""
        onBuildRuleCatchesNot "DL3053" "LABEL other=\"bar\""
    describe "Label is not SPDX license identifier rule" $
      let ?rulesConfig = Hadolint.Process.RulesConfig [] (Map.fromList [("spdxlabel", Rule.Spdx)]) False
       in do
      it "not ok with label not containing SPDX identifier" $ do
        ruleCatches "DL3054" "LABEL spdxlabel=\"not-spdx\""
        onBuildRuleCatches "DL3054" "LABEL spdxlabel=\"not-spdx\""
      it "ok with label containing SPDX identifier" $ do
        ruleCatchesNot "DL3054" "LABEL spdxlabel=\"BSD-3-Clause\""
        onBuildRuleCatchesNot "DL3054" "LABEL spdxlabel=\"MIT\""
      it "ok with other label not containing SPDX identifier" $ do
        ruleCatchesNot "DL3054" "LABEL other=\"fooo\""
        onBuildRuleCatchesNot "DL3054" "LABEL other=\"bar\""
    describe "Label is not git hash rule" $
      let ?rulesConfig = Hadolint.Process.RulesConfig [] (Map.fromList [("githash", Rule.GitHash)]) False
       in do
      it "not ok with label not containing git hash" $ do
        ruleCatches "DL3055" "LABEL githash=\"not-git-hash\""
        onBuildRuleCatches "DL3055" "LABEL githash=\"not-git-hash\""
      it "ok with label containing short git hash" $ do
        ruleCatchesNot "DL3055" "LABEL githash=\"2dbfae9\""
        onBuildRuleCatchesNot "DL3055" "LABEL githash=\"2dbfae9\""
      it "ok with label containing long git hash" $ do
        ruleCatchesNot "DL3055" "LABEL githash=\"43c572f1272b6b3171dd1db9e41b7027128ce080\""
        onBuildRuleCatchesNot "DL3055" "LABEL githash=\"43c572f1272b6b3171dd1db9e41b7027128ce080\""
      it "ok with other label not containing git hash" $ do
        ruleCatchesNot "DL3055" "LABEL other=\"foo\""
        onBuildRuleCatchesNot "DL3055" "LABEL other=\"bar\""
    describe "Label is not semantic version rule" $
      let ?rulesConfig = Hadolint.Process.RulesConfig [] (Map.fromList [("semver", Rule.SemVer)]) False
       in do
      it "not ok with label not containing semantic version" $ do
        ruleCatches "DL3056" "LABEL semver=\"not-sem-ver\""
        onBuildRuleCatches "DL3056" "LABEL semver=\"not-sem-ver\""
      it "ok with label containing semantic version" $ do
        ruleCatchesNot "DL3056" "LABEL semver=\"1.0.0\""
        onBuildRuleCatchesNot "DL3056" "LABEL semver=\"2.0.1-rc1\""
      it "ok with other label not containing semantic version" $ do
        ruleCatchesNot "DL3056" "LABEL other=\"foo\""
        onBuildRuleCatchesNot "DL3056" "LABEL other=\"bar\""
    --
    describe "Invalid Label Key Rule" $ do
      it "not ok with reserved namespace" $ do
        ruleCatches "DL3048" "LABEL com.docker.label=\"foo\""
        ruleCatches "DL3048" "LABEL io.docker.label=\"foo\""
        ruleCatches "DL3048" "LABEL org.dockerproject.label=\"foo\""
        onBuildRuleCatches "DL3048" "LABEL com.docker.label=\"foo\""
        onBuildRuleCatches "DL3048" "LABEL io.docker.label=\"foo\""
        onBuildRuleCatches "DL3048" "LABEL org.dockerproject.label=\"foo\""
      it "not ok with invalid character" $ do
        ruleCatches "DL3048" "LABEL invalid$character=\"foo\""
        onBuildRuleCatches "DL3048" "LABEL invalid$character=\"foo\""
      it "not ok with invalid start and end characters" $ do
        ruleCatches "DL3048" "LABEL .invalid =\"foo\""
        ruleCatches "DL3048" "LABEL -invalid =\"foo\""
        ruleCatches "DL3048" "LABEL 1invalid =\"foo\""
        onBuildRuleCatches "DL3048" "LABEL .invalid=\"foo\""
        onBuildRuleCatches "DL3048" "LABEL -invalid=\"foo\""
        onBuildRuleCatches "DL3048" "LABEL 1invalid=\"foo\""
      it "not ok with consecutive dividers" $ do
        ruleCatches "DL3048" "LABEL invalid..character=\"foo\""
        ruleCatches "DL3048" "LABEL invalid--character=\"foo\""
        onBuildRuleCatches "DL3048" "LABEL invalid..character=\"foo\""
        onBuildRuleCatches "DL3048" "LABEL invalid--character=\"foo\""
      it "ok with valid labels" $ do
        ruleCatchesNot "DL3048" "LABEL org.valid-key.label3=\"foo\""
        ruleCatchesNot "DL3048" "LABEL validlabel=\"foo\""
        onBuildRuleCatchesNot "DL3048" "LABEL org.valid-key.label3=\"foo\""
        onBuildRuleCatchesNot "DL3048" "LABEL validlabel=\"foo\""
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
         in ruleCatches "DL4006" $ Text.unlines dockerFile
      it "`ARG` can correctly unset variables" $
        let dockerFile =
              [ "ARG A_WITHOUT_EQ",
                "ARG A_WITH_EQ=",
                "RUN echo bla"
              ]
         in assertChecks
              (Text.unlines dockerFile)
              (assertBool "No Warnings or Errors should be triggered" . null)

    -- Run tests for the Config module
    ConfigSpec.tests
    -- Run tests for the Shell module
    ShellSpec.tests
    -- Run rule tests
    DL3045.tests
