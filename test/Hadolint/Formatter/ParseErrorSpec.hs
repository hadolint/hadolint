module Hadolint.Formatter.ParseErrorSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value, decode, withObject, (.:))
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Hadolint.Formatter (OutputFormat (CodeclimateJson, GitLabCodeclimateJson), write)
import Hadolint.Formatter.Format (Result (..), errorMessage)
import Hadolint.Formatter.TTY (formatError)
import Language.Docker.Parser
import System.IO.Silently (capture)
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

    it "uses the configured report path for parse errors in both Code Climate formats" $ do
      case parseText "RUNNN" of
        Right _ -> expectationFailure "expected a Dockerfile parse error"
        Left err -> do
          let results = [Result "input.Dockerfile" (Seq.singleton err) Seq.empty]
          forM_ [Nothing, Just "src/Dockerfile"] $ \override -> do
            let expected = Just (maybe "<string>" Text.pack override)
            (cc, _) <- capture (write [] [CodeclimateJson] False override results)
            let issue = decode (BSC.takeWhile (/= '\0') (BSC.pack cc))
            (issue >>= parseMaybe issuePath) `shouldBe` expected
            (gl, _) <- capture (write [] [GitLabCodeclimateJson] False override results)
            case decode (BSC.pack gl) of
              Just [entry] -> parseMaybe issuePath entry `shouldBe` expected
              _ -> expectationFailure "expected one GitLab issue"

issuePath :: Value -> Parser Text.Text
issuePath = withObject "issue" $ \issue -> do
  loc <- issue .: "location"
  loc .: "path"
