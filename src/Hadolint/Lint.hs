{-# LANGUAGE NamedFieldPuns #-}

module Hadolint.Lint where

import qualified Control.Concurrent.Async as Async
import Control.Parallel.Strategies (parListChunk, rseq, using)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Conc (numCapabilities)
import qualified Hadolint.Formatter.Checkstyle as Checkstyle
import qualified Hadolint.Formatter.Codacy as Codacy
import qualified Hadolint.Formatter.Codeclimate as Codeclimate
import qualified Hadolint.Formatter.Format as Format
import qualified Hadolint.Formatter.Json as Json
import qualified Hadolint.Formatter.TTY as TTY
import qualified Hadolint.Rules as Rules
import qualified Language.Docker as Docker
import Language.Docker.Parser (DockerfileError, Error)
import Language.Docker.Syntax (Dockerfile)
import ShellCheck.Interface (Severity (..))
import System.Exit (exitFailure, exitSuccess)

type ErrorRule = Text
type WarningRule = Text
type InfoRule = Text
type StyleRule = Text
type IgnoreRule = Text

type ExtraRule = Text

type TrustedRegistry = Text

data LintOptions = LintOptions
  { errorRules :: [ErrorRule],
    warningRules :: [WarningRule],
    infoRules :: [InfoRule],
    styleRules :: [StyleRule],
    ignoreRules :: [IgnoreRule],
    rulesConfig :: Rules.RulesConfig
  }
  deriving (Show)

data OutputFormat
  = Json
  | TTY
  | CodeclimateJson
  | GitlabCodeclimateJson
  | Checkstyle
  | Codacy
  deriving (Show, Eq)

printResultsAndExit :: OutputFormat -> Format.Result Text DockerfileError -> IO ()
printResultsAndExit format allResults = do
  printResult allResults
  if not . Format.isEmpty $ allResults
    then exitFailure
    else exitSuccess
  where
    printResult res =
      case format of
        TTY -> TTY.printResult res
        Json -> Json.printResult res
        Checkstyle -> Checkstyle.printResult res
        CodeclimateJson -> Codeclimate.printResult res >> exitSuccess
        GitlabCodeclimateJson -> Codeclimate.printGitlabResult res >> exitSuccess
        Codacy -> Codacy.printResult res >> exitSuccess

-- | Performs the process of parsing the dockerfile and analyzing it with all the applicable
-- rules, depending on the list of ignored rules.
-- Depending on the preferred printing format, it will output the results to stdout
lint :: LintOptions -> NonEmpty.NonEmpty String -> IO (Format.Result Text DockerfileError)
lint LintOptions {errorRules = errorList,
                  warningRules = warningList,
                  infoRules = infoList,
                  styleRules = styleList,
                  ignoreRules = ignoreList,
                  rulesConfig} dFiles = do
  parsedFiles <- Async.mapConcurrently parseFile (NonEmpty.toList dFiles)
  let results = lintAll parsedFiles `using` parListChunk (div numCapabilities 2) rseq
  return $ mconcat results
  where
    parseFile :: String -> IO (Either Error Dockerfile)
    parseFile "-" = Docker.parseStdin
    parseFile s = Docker.parseFile s

    lintAll = fmap lintDockerfile

    lintDockerfile = processedFile
      where
        processedFile = Format.toResult . fmap processRules
        processRules fileLines = filter ignoredRules $
                                 map (makeSeverity (Just ErrorC) errorList .
                                      makeSeverity (Just WarningC) warningList .
                                      makeSeverity (Just InfoC) infoList .
                                      makeSeverity (Just StyleC) styleList) $
                                 analyzeAll rulesConfig fileLines

        ignoredRules = ignoreFilter ignoreList

        makeSeverity s rules (Rules.RuleCheck (Rules.Metadata code severity message) filename linenumber success) =
          if code `elem` rules then
            Rules.RuleCheck (Rules.Metadata code s message) filename linenumber success
          else
            Rules.RuleCheck (Rules.Metadata code severity message) filename linenumber success

        ignoreFilter :: [IgnoreRule] -> Rules.RuleCheck -> Bool
        ignoreFilter rules (Rules.RuleCheck (Rules.Metadata code severity _) _ _ _) =
          code `notElem` rules && isJust severity

        extraFilter :: [ExtraRule] -> Rules.RuleCheck -> Bool
        extraFilter rules (Rules.RuleCheck (Rules.Metadata code _ _) _ _ _) = code `elem` rules

-- | Returns the result of applying all the rules to the given dockerfile
analyzeAll :: Rules.RulesConfig -> Dockerfile -> [Rules.RuleCheck]
analyzeAll config = Rules.analyze (Rules.rules ++ Rules.optionalRules config)

-- | Helper to analyze AST quickly in GHCI
analyzeEither :: Rules.RulesConfig -> Either t Dockerfile -> [Rules.RuleCheck]
analyzeEither _ (Left _) = []
analyzeEither config (Right dockerFile) = analyzeAll config dockerFile
