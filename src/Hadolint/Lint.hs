{-# LANGUAGE NamedFieldPuns #-}

module Hadolint.Lint where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Language.Docker as Docker
import Language.Docker.Parser (DockerfileError, Error)
import Language.Docker.Syntax (Dockerfile)
import System.Exit (exitFailure, exitSuccess)

import qualified Hadolint.Formatter.Checkstyle as Checkstyle
import qualified Hadolint.Formatter.Codacy as Codacy
import qualified Hadolint.Formatter.Codeclimate as Codeclimate
import qualified Hadolint.Formatter.Format as Format
import qualified Hadolint.Formatter.Json as Json
import qualified Hadolint.Formatter.TTY as TTY
import qualified Hadolint.Rules as Rules

type IgnoreRule = Text

type TrustedRegistry = Text

data LintOptions = LintOptions
    { ignoreRules :: [IgnoreRule]
    , rulesConfig :: Rules.RulesConfig
    } deriving (Show)

data OutputFormat
    = Json
    | TTY
    | CodeclimateJson
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
            Codacy -> Codacy.printResult res >> exitSuccess

-- | Performs the process of parsing the dockerfile and analyzing it with all the applicable
-- rules, depending on the list of ignored rules.
-- Depending on the preferred printing format, it will output the results to stdout
lint :: LintOptions -> NonEmpty.NonEmpty String -> IO (Format.Result Text DockerfileError)
lint LintOptions {ignoreRules = ignoreList, rulesConfig} dFiles = do
    processedFiles <- mapM (lintDockerfile ignoreList) (NonEmpty.toList dFiles)
    return (results processedFiles)
  where
    results = foldMap Format.toResult -- Parse and check rules for each dockerfile,
                                      -- then convert them to a Result and combine with
                                      -- the result of the previous dockerfile results
    lintDockerfile ignoreRules dockerFile = do
        ast <- parseFilename dockerFile
        return (processedFile ast)
      where
        processedFile = fmap processRules
        processRules fileLines = filter ignoredRules (analyzeAll rulesConfig fileLines)
        ignoredRules = ignoreFilter ignoreRules
        -- | Returns true if the rule should be ignored
        ignoreFilter :: [IgnoreRule] -> Rules.RuleCheck -> Bool
        ignoreFilter rules (Rules.RuleCheck (Rules.Metadata code _ _) _ _ _) = code `notElem` rules
        -- | Support UNIX convention of passing "-" instead of "/dev/stdin"
        parseFilename :: String -> IO (Either Error Dockerfile)
        parseFilename "-" = Docker.parseStdin
        parseFilename s = Docker.parseFile s

-- | Returns the result of applying all the rules to the given dockerfile
analyzeAll :: Rules.RulesConfig -> Dockerfile -> [Rules.RuleCheck]
analyzeAll config = Rules.analyze (Rules.rules ++ Rules.optionalRules config)

-- | Helper to analyze AST quickly in GHCI
analyzeEither :: Rules.RulesConfig -> Either t Dockerfile -> [Rules.RuleCheck]
analyzeEither _ (Left _) = []
analyzeEither config (Right dockerFile) = analyzeAll config dockerFile
