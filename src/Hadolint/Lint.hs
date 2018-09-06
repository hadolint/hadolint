{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hadolint.Lint where

import Data.Text (Text)
import System.Exit (exitFailure, exitSuccess)
import qualified Paths_hadolint -- version from hadolint.cabal file
import qualified Data.Version
import qualified Development.GitRev
import qualified Language.Docker as Docker
import Language.Docker.Syntax (Dockerfile)
import qualified Hadolint.Rules as Rules
import qualified Hadolint.Formatter.Checkstyle as Checkstyle
import qualified Hadolint.Formatter.Codeclimate as Codeclimate
import qualified Hadolint.Formatter.Format as Format
import qualified Hadolint.Formatter.Json as Json
import qualified Hadolint.Formatter.TTY as TTY
import qualified Hadolint.Formatter.Codacy as Codacy

type IgnoreRule = Text

data LintOptions = LintOptions
    { showVersion :: Bool
    , configFile :: Maybe FilePath
    , format :: OutputFormat
    , ignoreRules :: [IgnoreRule]
    , dockerfiles :: [String]
    , rulesConfig :: Rules.RulesConfig
    } deriving (Show)

data OutputFormat
    = Json
    | TTY
    | CodeclimateJson
    | Checkstyle
    | Codacy
    deriving (Show, Eq)

ignoreFilter :: [IgnoreRule] -> Rules.RuleCheck -> Bool
ignoreFilter ignoredRules (Rules.RuleCheck (Rules.Metadata code _ _) _ _ _) =
    code `notElem` ignoredRules
    
-- | Performs the process of parsing the dockerfile and analyzing it with all the applicable
-- rules, depending on the list of ignored rules.
-- Depending on the preferred printing format, it will output the results to stdout
lint :: LintOptions -> IO ()
lint LintOptions {showVersion = True} = putStrLn getVersion >> exitSuccess
lint LintOptions {dockerfiles = []} = putStrLn "Please provide a Dockerfile" >> exitFailure
lint LintOptions {ignoreRules = ignoreList, dockerfiles = dFiles, format, rulesConfig} = do
    processedFiles <- mapM (lintDockerfile ignoreList) dFiles
    let allResults = results processedFiles
    printResult allResults
    if allResults /= mempty
        then exitFailure
        else exitSuccess
  where
    results = foldMap Format.toResult -- Parse and check rules for each dockerfile,
                                      -- then convert them to a Result and combine with
                                      -- the result of the previous dockerfile results
    printResult res =
        case format of
            TTY -> TTY.printResult res
            Json -> Json.printResult res
            Checkstyle -> Checkstyle.printResult res
            CodeclimateJson -> Codeclimate.printResult res >> exitSuccess
            Codacy -> Codacy.printResult res >> exitSuccess
    lintDockerfile ignoreRules dockerFile = do
        ast <- Docker.parseFile (parseFilename dockerFile)
        return (processedFile ast)
      where
        processedFile = fmap processRules
        processRules fileLines = filter ignoredRules (analyzeAll rulesConfig fileLines)
        ignoredRules = ignoreFilter ignoreRules

-- | Support UNIX convention of passing "-" instead of "/dev/stdin"
parseFilename :: String -> String
parseFilename "-" = "/dev/stdin"
parseFilename s = s

getVersion :: String
getVersion
    | $(Development.GitRev.gitDescribe) == "UNKNOWN" =
        "Haskell Dockerfile Linter " ++ Data.Version.showVersion Paths_hadolint.version ++ "-no-git"
    | otherwise = "Haskell Dockerfile Linter " ++ $(Development.GitRev.gitDescribe)

-- | Returns the result of applying all the rules to the given dockerfile
analyzeAll :: Rules.RulesConfig -> Dockerfile -> [Rules.RuleCheck]
analyzeAll config = Rules.analyze (Rules.rules ++ Rules.optionalRules config)
