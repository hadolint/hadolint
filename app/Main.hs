{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Hadolint.Rules
import Language.Docker.Parser
import Language.Docker.Syntax

import Control.Applicative
import Control.Monad (filterM)
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Semigroup ((<>))
import qualified Data.Version as V (showVersion)
import qualified Data.Yaml as Yaml
import Development.GitRev (gitDescribe)
import GHC.Generics
import Options.Applicative hiding (ParseError)
import Paths_hadolint (version) -- version from hadolint.cabal file
import System.Directory
       (XdgDirectory(..), doesFileExist, getCurrentDirectory,
        getXdgDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import Text.Parsec (ParseError)

import qualified Hadolint.Formatter.Codeclimate as Codeclimate
import Hadolint.Formatter.Format (toResult)
import qualified Hadolint.Formatter.Json as Json
import qualified Hadolint.Formatter.TTY as TTY

type IgnoreRule = String

data OutputFormat
    = Json
    | Plain
    | CodeclimateJson
    deriving (Show, Eq)

data LintOptions = LintOptions
    { showVersion :: Bool
    , format :: OutputFormat
    , ignoreRules :: [IgnoreRule]
    , dockerfiles :: [String]
    } deriving (Show)

newtype ConfigFile = ConfigFile
    { ignored :: [IgnoreRule]
    } deriving (Show, Eq, Generic)

instance Yaml.FromJSON ConfigFile

ignoreFilter :: [IgnoreRule] -> RuleCheck -> Bool
ignoreFilter ignoredRules (RuleCheck (Metadata code _ _) _ _ _) = code `notElem` ignoredRules

toOutputFormat :: String -> Maybe OutputFormat
toOutputFormat "json" = Just Json
toOutputFormat "text" = Just Plain
toOutputFormat "codeclimate" = Just CodeclimateJson
toOutputFormat _ = Nothing

showFormat :: OutputFormat -> String
showFormat Json = "json"
showFormat Plain = "text"
showFormat CodeclimateJson = "codeclimate"

parseOptions :: Parser LintOptions
parseOptions =
    LintOptions <$> -- CLI options parser definition
    version <*>
    outputFormat <*>
    ignoreList <*>
    files
  where
    version = switch (long "version" <> short 'v' <> help "Show version")
    --
    -- | Parse the output format option
    outputFormat =
        option
            (maybeReader toOutputFormat)
            (long "format" <> -- options for the output format
             short 'f' <>
             help "The output format for the results [text | json | codeclimate]" <>
             value Plain <> -- The default value
             showDefaultWith showFormat <>
             completeWith ["text", "json", "codeclimate"])
    --
    -- | Parse a list of ignored rules
    ignoreList =
        many
            (strOption
                 (long "ignore" <> help "Ignore rule. If present, config file is ignored" <>
                  metavar "RULECODE"))
    --
    -- | Parse a list of dockerfile names
    files = many (argument str (metavar "DOCKERFILE..." <> action "file"))

main :: IO ()
main = execParser opts >>= applyConfig >>= lint
  where
    opts =
        info
            (helper <*> parseOptions)
            (fullDesc <> progDesc "Lint Dockerfile for errors and best practices" <>
             header "hadolint - Dockerfile Linter written in Haskell")

applyConfig :: LintOptions -> IO LintOptions
applyConfig o@LintOptions {ignoreRules = (_:_)} = return o
applyConfig o = do
    theConfig <- findConfig
    case theConfig of
        Nothing -> return o
        Just config -> parseAndApply config
  where
    findConfig = do
        localConfigFile <- (</> ".hadolint.yaml") <$> getCurrentDirectory
        configFile <- getXdgDirectory XdgConfig "hadolint.yaml"
        listToMaybe <$> filterM doesFileExist [localConfigFile, configFile]
    parseAndApply config = do
        result <- Yaml.decodeFileEither config
        case result of
            Left err -> printError err config
            Right (ConfigFile ignore) -> return o {ignoreRules = ignore}
    printError err config =
        case err of
            Yaml.AesonException _ ->
                error $
                "Error parsing your config file in  '" ++
                config ++
                "':\nIt should contain an 'ignored' key with a list of strings. For example:\n\n" ++
                unlines ["ignored:", "\t- DL3000", "\t- SC1099"]
            _ ->
                error $
                "Error parsing your config file in  '" ++
                config ++ "': " ++ Yaml.prettyPrintParseException err

-- | Support UNIX convention of passing "-" instead of "/dev/stdin"
parseFilename :: String -> String
parseFilename "-" = "/dev/stdin"
parseFilename s = s

lintDockerfile :: [IgnoreRule] -> String -> IO (Either ParseError [RuleCheck])
lintDockerfile ignoreRules dockerfile = do
    ast <- parseFile $ parseFilename dockerfile
    return (processedFile ast)
  where
    processedFile = fmap processRules
    processRules dockerfile = filter ignored (analyzeAll dockerfile)
    ignored = ignoreFilter ignoreRules

getVersion :: String
getVersion
    | $(gitDescribe) == "UNKNOWN" =
        "Haskell Dockerfile Linter " ++ V.showVersion version ++ "-no-git"
    | otherwise = "Haskell Dockerfile Linter " ++ $(gitDescribe)

lint :: LintOptions -> IO ()
lint LintOptions {showVersion = True} = putStrLn getVersion >> exitSuccess
lint LintOptions {dockerfiles = []} = putStrLn "Please provide a Dockerfile" >> exitFailure
lint LintOptions {ignoreRules = ignoreList, dockerfiles = dFiles, format} = do
    processedFiles <- mapM (lintDockerfile ignoreList) dFiles
    let allResults = results processedFiles
    printResult allResults
    if allResults /= mempty
        then exitFailure
        else exitSuccess
  where
    results = foldMap toResult -- Parse and check rules for each dockerfile,
                               -- then convert them to a Result and combine with
                               -- the result of the previous dockerfile results
    printResult res =
        case format of
            Plain -> TTY.printResult res
            Json -> Json.printResult res
            CodeclimateJson -> Codeclimate.printResult res >> exitSuccess

analyzeAll :: Dockerfile -> [RuleCheck]
analyzeAll = analyze rules

-- Helper to analyze AST quickly in GHCI
analyzeEither :: Either t Dockerfile -> [RuleCheck]
analyzeEither (Left err) = []
analyzeEither (Right dockerfile) = analyzeAll dockerfile
