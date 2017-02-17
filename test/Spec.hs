import Test.Hspec
import Test.HUnit hiding (Label)

import Hadolint.Formatter
import Hadolint.Parser
import Hadolint.Rules
import Hadolint.Syntax
import Hadolint.Normalize

import Data.List (find)
import Data.Maybe (isJust, fromMaybe)

main :: IO ()
main = hspec $ do
  describe "parse HEALTHCHECK" $
    it "parse healthcheck without args" $
        assertAst "HEALTHCHECK --interval=5m \\nCMD curl -f http://localhost/" [Healthcheck "--interval=5m \\nCMD curl -f http://localhost/"]

  describe "parse FROM" $
    it "parse untagged image" $
        assertAst "FROM busybox" [From (UntaggedImage "busybox")]

  describe "parse LABEL" $ do
    it "parse label" $ assertAst "LABEL foo=bar" [Label[("foo", "bar")]]
    it "parses multiline labels" $
        let dockerfile = unlines [ "LABEL foo=bar \\", "hobo=mobo"]
            ast = [ Label[("foo", "bar"), ("hobo", "mobo")] ]
        in assertAst dockerfile ast


  describe "parse ENV" $ do
    it "parses unquoted pair" $
        assertAst "ENV foo=bar" [Env [("foo", "bar")]]
    it "parse with space between key and value" $
        assertAst "ENV foo bar" [Env [("foo", "bar")] ]
    it "parse quoted value pair" $
        assertAst "ENV foo=\"bar\"" [Env [("foo", "bar")]]
    it "parse multiple unquoted pairs" $
        assertAst "ENV foo=bar baz=foo" [Env [("foo", "bar"), ("baz", "foo")]]
    it "parse multiple quoted pairs" $
        assertAst "ENV foo=\"bar\" baz=\"foo\"" [Env [("foo", "bar"), ("baz", "foo")]]
    it "env works before cmd" $
        let dockerfile = "ENV PATH=\"/root\"\nCMD [\"hadolint\",\"-i\"]"
            ast = [ Env [("PATH", "/root")]
                  , Cmd ["hadolint", "-i"]
                  ]
        in assertAst dockerfile ast
    it "parse with two spaces between" $
        let dockerfile = "ENV NODE_VERSION=v5.7.1  DEBIAN_FRONTEND=noninteractive"
        in assertAst dockerfile [Env[("NODE_VERSION", "v5.7.1"), ("DEBIAN_FRONTEND", "noninteractive")]]
    it "have envs on multiple lines" $
        let dockerfile = unlines [ "FROM busybox"
                                 , "ENV NODE_VERSION=v5.7.1 \\"
                                 , "DEBIAN_FRONTEND=noninteractive"
                                 ]
            ast = [ From (UntaggedImage "busybox")
                  , Env[("NODE_VERSION", "v5.7.1"), ("DEBIAN_FRONTEND", "noninteractive")]
                  ]
        in assertAst dockerfile ast
    it "parses long env over multiple lines" $
        let dockerfile = unlines [ "ENV LD_LIBRARY_PATH=\"/usr/lib/\" \\"
                                 , "APACHE_RUN_USER=\"www-data\" APACHE_RUN_GROUP=\"www-data\""]
            ast = [Env[("LD_LIBRARY_PATH", "/usr/lib/")
                      ,("APACHE_RUN_USER", "www-data")
                      ,("APACHE_RUN_GROUP", "www-data")]]
            in assertAst dockerfile ast

  describe "parse RUN" $
    it "escaped with space before" $
        let dockerfile = unlines [ "RUN yum install -y \\ "
                             , "imagemagick \\ "
                             , "mysql"
                             ]
        in assertAst dockerfile [Run ["yum", "install", "-y", "imagemagick", "mysql"]]

  describe "parse CMD" $ do
    it "one line cmd" $
        assertAst "CMD true" [Cmd ["true"]]
    it "cmd over several lines" $
        assertAst "CMD true \\\n && true" [Cmd ["true", "&&", "true"]]
    it "quoted command params" $
        assertAst "CMD [\"echo\",  \"1\"]" [Cmd ["echo", "1"]]

  describe "parse SHELL" $
    it "quoted shell params" $
        assertAst "SHELL [\"/bin/bash\",  \"-c\"]" [Shell ["/bin/bash", "-c"]]

  describe "parse MAINTAINER" $ do
    it "maintainer of untagged scratch image" $
        assertAst "FROM scratch\nMAINTAINER hudu@mail.com" [From (UntaggedImage "scratch"), Maintainer "hudu@mail.com"]
    it "maintainer with mail" $
        assertAst "MAINTAINER hudu@mail.com" [Maintainer "hudu@mail.com"]
    it "maintainer only mail after from" $
        let maintainerFromProg = "FROM busybox\nMAINTAINER hudu@mail.com"
            maintainerFromAst = [ From (UntaggedImage "busybox")
                                , Maintainer "hudu@mail.com"
                                ]
        in assertAst maintainerFromProg maintainerFromAst

  describe "parse # comment " $ do
      it "multiple comments before run" $
        let dockerfile = unlines [ "# line 1"
                                 , "# line 2"
                                 , "RUN apt-get update"
                                 ]
        in assertAst dockerfile [Run ["apt-get", "update"]]
      it "multiple comments after run" $
        let dockerfile = unlines [ "RUN apt-get update"
                                 , "# line 1"
                                 , "# line 2"
                                 ]
        in assertAst dockerfile [Run ["apt-get", "update"], Comment " line 1", Comment " line 2"]

  describe "normalize lines" $ do
    it "join multiple ENV" $
        let dockerfile = unlines [ "FROM busybox"
                                 , "ENV NODE_VERSION=v5.7.1 \\"
                                 , "DEBIAN_FRONTEND=noninteractive"
                                 ]
            normalizedDockerfile = unlines [ "FROM busybox"
                                           , "ENV NODE_VERSION=v5.7.1  DEBIAN_FRONTEND=noninteractive\n"]
        in normalizeEscapedLines dockerfile `shouldBe` normalizedDockerfile
    it "join escaped lines" $
        let dockerfile           = unlines ["ENV foo=bar \\", "baz=foz"]
            normalizedDockerfile = unlines ["ENV foo=bar  baz=foz", ""]
        in normalizeEscapedLines dockerfile `shouldBe` normalizedDockerfile
    it "join long CMD" $
        let longEscapedCmd = unlines
                            [ "RUN wget https://download.com/${version}.tar.gz -O /tmp/logstash.tar.gz && \\"
                            , "(cd /tmp && tar zxf logstash.tar.gz && mv logstash-${version} /opt/logstash && \\"
                            , "rm logstash.tar.gz) && \\"
                            , "(cd /opt/logstash && \\"
                            ,  "/opt/logstash/bin/plugin install contrib)"
                            ]
            longEscapedCmdExpected = concat
              [ "RUN wget https://download.com/${version}.tar.gz -O /tmp/logstash.tar.gz &&  "
              , "(cd /tmp && tar zxf logstash.tar.gz && mv logstash-${version} /opt/logstash &&  "
              , "rm logstash.tar.gz) &&  "
              , "(cd /opt/logstash &&  "
              ,  "/opt/logstash/bin/plugin install contrib)\n"
              , "\n"
              , "\n"
              , "\n"
              , "\n"
              ]
        in normalizeEscapedLines longEscapedCmd `shouldBe` longEscapedCmdExpected

  describe "FROM rules" $ do
    it "no untagged" $ ruleCatches noUntagged "FROM debian"
    it "explicit latest" $ ruleCatches noLatestTag "FROM debian:latest"
    it "explicit tagged" $ ruleCatchesNot noLatestTag "FROM debian:jessie"

  describe "no root or sudo rules" $ do
    it "sudo" $ ruleCatches noSudo "RUN sudo apt-get update"
    it "no root" $ ruleCatches noRootUser "USER root"
    it "install sudo" $ ruleCatchesNot noSudo "RUN apt-get install sudo"
    it "sudo chained programs" $ ruleCatches noSudo "RUN apt-get update && sudo apt-get install"

  describe "invalid CMD rules" $ do
    it "invalid cmd" $ ruleCatches invalidCmd "RUN top"
    it "install ssh" $ ruleCatchesNot invalidCmd "RUN apt-get install ssh"

  describe "apt-get rules" $ do
    it "apt upgrade" $ ruleCatches noUpgrade "RUN apt-get update && apt-get upgrade"
    it "apt-get version pinning" $ ruleCatches aptGetVersionPinned "RUN apt-get update && apt-get install python"
    it "apt-get no cleanup" $ ruleCatches aptGetCleanup "RUN apt-get update && apt-get install python"
    it "apt-get cleanup" $ ruleCatchesNot aptGetCleanup "RUN apt-get update && apt-get install python && rm -rf /var/lib/apt/lists/*"
    it "apt-get pinned chained" $
      let dockerfile = [ "RUN apt-get update \\"
                         , " && apt-get -y --no-install-recommends install nodejs=0.10 \\"
                         , " && rm -rf /var/lib/apt/lists/*"
                       ]
      in ruleCatchesNot aptGetVersionPinned $ unlines dockerfile

    it "apt-get pinned regression" $
        let dockerfile = [ "RUN apt-get update && apt-get install --no-install-recommends -y \\"
                         , "python-demjson=2.2.2* \\"
                         , "wget=1.16.1* \\"
                         , "git=1:2.5.0* \\"
                         , "ruby=1:2.1.*"
                         ]
        in ruleCatchesNot aptGetVersionPinned $ unlines dockerfile

    it "has deprecated maintainer" $ ruleCatches hasNoMaintainer "FROM busybox\nMAINTAINER hudu@mail.com"

  describe "EXPOSE rules" $ do
    it "has no arg" $ ruleCatches exposeMissingArgs "EXPOSE"
    it "has one arg" $ ruleCatchesNot exposeMissingArgs "EXPOSE 80"
    it "invalid port" $ ruleCatches invalidPort "EXPOSE 80000"
    it "valid port" $ ruleCatchesNot invalidPort "EXPOSE 60000"

  describe "pip pinning" $ do
    it "pip2 version not pinned" $ ruleCatches pipVersionPinned "RUN pip2 install MySQL_python"
    it "pip3 version not pinned" $ ruleCatches pipVersionPinned "RUN pip3 install MySQL_python"
    it "pip3 version pinned" $ ruleCatchesNot pipVersionPinned "RUN pip3 install MySQL_python==1.2.2"
    it "pip install requirements" $ ruleCatchesNot pipVersionPinned "RUN pip install -r requirements.txt"
    it "pip version not pinned" $ ruleCatches pipVersionPinned "RUN pip install MySQL_python"
    it "pip version pinned" $ ruleCatchesNot pipVersionPinned "RUN pip install MySQL_python==1.2.2"
    it "pip install git" $ ruleCatchesNot pipVersionPinned "RUN pip install git+https://github.com/rtfd/readthedocs-sphinx-ext.git@0.6-alpha#egg=readthedocs-sphinx-ext"
    it "pip install unversioned git" $ ruleCatches pipVersionPinned "RUN pip install git+https://github.com/rtfd/readthedocs-sphinx-ext.git#egg=readthedocs-sphinx-ext"
    it "pip install upper bound" $ ruleCatchesNot pipVersionPinned "RUN pip install 'alabaster>=0.7'"
    it "pip install lower bound" $ ruleCatchesNot pipVersionPinned "RUN pip install 'alabaster<0.7'"
    it "pip install excluded version" $ ruleCatchesNot pipVersionPinned "RUN pip install 'alabaster!=0.7'"

  describe "use SHELL" $ do
    it "RUN ln" $ ruleCatches useShell "RUN ln -sfv /bin/bash /bin/sh"
    it "RUN ln with unrelated symlinks" $ ruleCatchesNot useShell "RUN ln -sf /bin/true /sbin/initctl"
    it "RUN ln with multiple acceptable commands" $ ruleCatchesNot useShell "RUN ln -s foo bar && unrelated && something_with /bin/sh"

  describe "COPY rules" $ do
    it "has source" $ ruleCatches copyMissingArgs "COPY packaged-app.tar"
    it "has source and target" $ ruleCatchesNot copyMissingArgs "COPY packaged-app.tar /usr/src/app"
    it "use add" $ ruleCatches useAdd "COPY packaged-app.tar /usr/src/app"
    it "use not add" $ ruleCatchesNot useAdd "COPY package.json /usr/src/app"

  describe "other rules" $ do
    it "apt-get auto yes" $ ruleCatches aptGetYes "RUN apt-get install python"
    it "apt-get yes shortflag" $ ruleCatchesNot aptGetYes "RUN apt-get install -yq python"
    it "apt-get yes different pos" $ ruleCatchesNot aptGetYes "RUN apt-get install -y python"
    it "apt-get with auto yes" $ ruleCatchesNot aptGetYes "RUN apt-get -y install python"
    it "apt-get with auto expanded yes" $ ruleCatchesNot aptGetYes "RUN apt-get --yes install python"
    it "apt-get install recommends" $ ruleCatchesNot aptGetNoRecommends "RUN apt-get install --no-install-recommends python"
    it "apt-get no install recommends" $ ruleCatches aptGetNoRecommends "RUN apt-get install python"
    it "apt-get no install recommends" $ ruleCatches aptGetNoRecommends "RUN apt-get -y install python"
    it "apt-get version" $ ruleCatchesNot aptGetVersionPinned "RUN apt-get install -y python=1.2.2"
    it "apt-get pinned" $ ruleCatchesNot aptGetVersionPinned "RUN apt-get -y --no-install-recommends install nodejs=0.10"
    it "has maintainer" $ ruleCatches hasNoMaintainer "FROM debian\nMAINTAINER Lukas"
    it "has maintainer first" $ ruleCatches hasNoMaintainer "MAINTAINER Lukas\nFROM DEBIAN"
    it "has no maintainer" $ ruleCatchesNot hasNoMaintainer "FROM debian"
    it "using add" $ ruleCatches copyInsteadAdd "ADD file /usr/src/app/"
    it "many cmds" $ ruleCatches multipleCmds "CMD /bin/true\nCMD /bin/true"
    it "single cmd" $ ruleCatchesNot multipleCmds "CMD /bin/true"
    it "no cmd" $ ruleCatchesNot multipleEntrypoints "FROM busybox"
    it "many entries" $ ruleCatches multipleEntrypoints "ENTRYPOINT /bin/true\nENTRYPOINT /bin/true"
    it "single entry" $ ruleCatchesNot multipleEntrypoints "ENTRYPOINT /bin/true"
    it "no entry" $ ruleCatchesNot multipleEntrypoints "FROM busybox"
    it "workdir variable" $ ruleCatchesNot absoluteWorkdir "WORKDIR ${work}"
    it "scratch" $ ruleCatchesNot noUntagged "FROM scratch"

  describe "add files and archives" $ do
    it "add for tar" $ ruleCatchesNot copyInsteadAdd "ADD file.tar /usr/src/app/"
    it "add for zip" $ ruleCatchesNot copyInsteadAdd "ADD file.zip /usr/src/app/"
    it "add for gzip" $ ruleCatchesNot copyInsteadAdd "ADD file.gz /usr/src/app/"
    it "add for bz2" $ ruleCatchesNot copyInsteadAdd "ADD file.bz2 /usr/src/app/"
    it "add for xz" $ ruleCatchesNot copyInsteadAdd "ADD file.xz /usr/src/app/"
    it "add for tgz" $ ruleCatchesNot copyInsteadAdd "ADD file.tgz /usr/src/app/"
    it "add for url" $ ruleCatchesNot copyInsteadAdd "ADD http://file.com /usr/src/app/"

  describe "format error" $
    it "display error after line pos" $ do
        let ast = parseString "FOM debian:jessie"
            expectedMsg = "<string>:1:1 unexpected 'F' expecting space, \"\\t\", \"ONBUILD\", \"FROM\", \"COPY\", \"RUN\", \"WORKDIR\", \"ENTRYPOINT\", \"VOLUME\", \"EXPOSE\", \"ENV\", \"ARG\", \"USER\", \"LABEL\", \"STOPSIGNAL\", \"CMD\", \"SHELL\", \"MAINTAINER\", \"ADD\", \"#\", \"HEALTHCHECK\" or end of input"
        case ast of
            Left err -> assertEqual "Unexpected error msg" expectedMsg (formatError err)
            Right _  -> assertFailure "AST should fail parsing"

assertAst s ast = case parseString (s ++ "\n") of
    Left err          -> assertFailure $ show err
    Right dockerfile  -> assertEqual "ASTs are not equal" ast $ map instruction dockerfile

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
