{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Hadolint.Rules
import Language.Docker.Parser
import Language.Docker.Syntax

import Control.Applicative
import Control.Monad (filterM)
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Semigroup
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

import qualified Hadolint.Formatter.Json as Json
import qualified Hadolint.Formatter.TTY as TTY

type IgnoreRule = String

data LintOptions = LintOptions
    { showVersion :: Bool
    , ignoreRules :: [IgnoreRule]
    , dockerfiles :: [String]
    } deriving (Show)

newtype ConfigFile = ConfigFile
    { ignored :: [IgnoreRule]
    } deriving (Show, Eq, Generic)

instance Yaml.FromJSON ConfigFile

ignoreFilter :: [IgnoreRule] -> RuleCheck -> Bool
ignoreFilter ignoredRules (RuleCheck (Metadata code _ _) _ _ _) = code `notElem` ignoredRules

parseOptions :: Parser LintOptions
parseOptions =
    LintOptions <$> -- CLI options parser definition
    switch (long "version" <> short 'v' <> help "Show version") <*>
    many
        (strOption
             (long "ignore" <> help "Ignore rule. If present, config file is ignored" <>
              metavar "RULECODE")) <*>
    many (argument str (metavar "DOCKERFILE..."))

main :: IO ()
main = execParser opts >>= applyConfig >>= lint
  where
    opts =
        info
            (helper <*> parseOptions)
            (fullDesc <> progDesc "Lint Dockerfile for errors and best practices" <>
             header "hadolint - Dockerfile Linter written in Haskell")

applyConfig :: LintOptions -> IO LintOptions
applyConfig o@(LintOptions _ (_:_) _) = return o
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

lintDockerfile :: [IgnoreRule] -> String -> IO ()
lintDockerfile ignoreRules dockerfile = do
    ast <- parseFile $ parseFilename dockerfile
    checkAst (ignoreFilter ignoreRules) ast

getVersion :: String
getVersion
    | $(gitDescribe) == "UNKNOWN" =
        "Haskell Dockerfile Linter " ++ V.showVersion version ++ "-no-git"
    | otherwise = "Haskell Dockerfile Linter " ++ $(gitDescribe)

lint :: LintOptions -> IO ()
lint (LintOptions True _ _) = putStrLn getVersion >> exitSuccess
lint (LintOptions _ _ []) = putStrLn "Please provide a Dockerfile" >> exitFailure
lint (LintOptions _ ignore dfiles) = mapM_ (lintDockerfile ignore) dfiles

checkAst :: (RuleCheck -> Bool) -> Either ParseError Dockerfile -> IO ()
checkAst checkFilter ast = printResults >> exitWithSignal
  where
    printResults = TTY.printResults processedFile
    exitWithSignal = either (const exitFailure) (const exitSuccess) ast
    processedFile = fmap processRules ast
    processRules dockerfile = filter checkFilter (analyzeAll dockerfile)

analyzeAll :: Dockerfile -> [RuleCheck]
analyzeAll = analyze rules

-- Helper to analyze AST quickly in GHCI
analyzeEither :: Either t Dockerfile -> [RuleCheck]
analyzeEither (Left err) = []
analyzeEither (Right dockerfile) = analyzeAll dockerfile
