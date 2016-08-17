import Test.Hspec
import Test.HUnit

import Hadolint.Parser
import Hadolint.Rules
import Hadolint.Syntax
import Hadolint.Normalize

import Data.List (find)
import Data.Maybe (isJust, fromMaybe)

main :: IO ()
main = hspec $ do
  describe "FROM" $
    it "parse untagged image" $
        assertAst "FROM busybox" [From (UntaggedImage "busybox")]

  describe "ENV" $ do
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

  describe "RUN" $
    it "escaped with space before" $
        let dockerfile = unlines [ "RUN yum install -y \\ "
                             , "imagemagick \\ "
                             , "mysql"
                             ]
        in assertAst dockerfile [Run ["yum", "install", "-y", "imagemagick", "mysql"]]

  describe "CMD" $ do
    it "one line cmd" $
        assertAst "CMD true" [Cmd ["true"]]
    it "cmd over several lines" $
        assertAst "CMD true \\\n && true" [Cmd ["true", "&&", "true"]]
    it "quoted command params" $
        assertAst "CMD [\"echo\",  \"1\"]" [Cmd ["echo", "1"]]

  describe "MAINTAINER" $ do
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

  describe "COMMENT" $ do
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
