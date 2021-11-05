module Hadolint.Formatter.ParseErrorSpec (spec) where

import Hadolint.Formatter.Format (errorMessage)
import Hadolint.Formatter.TTY (formatError)
import Language.Docker.Parser
import Test.HUnit hiding (Label)
import Test.Hspec


spec :: SpecWith ()
spec = do
  describe "Parse Errors" $ do
    it "display error after line pos" $ do
      let ast = parseText "FOM debian:jessie"
          expectedMsg =
            "<string>:1:1 unexpected 'F' expecting '#', ADD, ARG, CMD, COPY,\
            \ ENTRYPOINT, ENV, EXPOSE, FROM, HEALTHCHECK, LABEL, MAINTAINER,\
            \ ONBUILD, RUN, SHELL, STOPSIGNAL, USER, VOLUME, WORKDIR, a pragma,\
            \ end of input, or whitespaces "
      case ast of
        Left err ->
          assertEqual "Unexpected error msg" expectedMsg (formatError err)
        Right _ -> assertFailure "AST should fail parsing"

    it "display just the error message" $ do
      let ast = parseText "RUNNN"
          expectedMsg = "missing whitespace"
      case ast of
        Left err ->
          assertEqual "Unexpected error msg" expectedMsg (errorMessage err)
        Right _ -> assertFailure "AST should fail parsing"
