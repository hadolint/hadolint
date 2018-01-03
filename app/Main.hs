{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hadolint.Formatter
import Hadolint.Rules
import Language.Docker.Parser
import Language.Docker.Syntax

import Control.Applicative
import Data.List (sort)
import Data.Semigroup
import qualified Data.Version as V (showVersion)
import Development.GitRev (gitDescribe)
import Options.Applicative hiding (ParseError)
import Paths_hadolint (version) -- version from hadolint.cabal file
import System.Environment (getArgs)
import System.Exit hiding (die)
import Text.Parsec (ParseError)

type IgnoreRule = String

data LintOptions = LintOptions
    { showVersion :: Bool
    , ignoreRules :: [IgnoreRule]
    , dockerfiles :: [String]
    }

ignoreFilter :: [IgnoreRule] -> RuleCheck -> Bool
ignoreFilter ignoredRules (RuleCheck (Metadata code _ _) _ _ _) = code `notElem` ignoredRules

printChecks :: [RuleCheck] -> IO ()
printChecks checks = do
    mapM_ (putStrLn . formatCheck) $ sort checks
    if null checks
        then exitSuccess
        else die

parseOptions :: Parser LintOptions
parseOptions =
    LintOptions <$> switch (long "version" <> short 'v' <> help "Show version") <*>
    many (strOption (long "ignore" <> help "Ignore rule" <> metavar "RULECODE")) <*>
    many (argument str (metavar "DOCKERFILE..."))

main :: IO ()
main = execParser opts >>= lint
  where
    opts =
        info
            (helper <*> parseOptions)
            (fullDesc <> progDesc "Lint Dockerfile for errors and best practices" <>
             header "hadolint - Dockerfile Linter written in Haskell")

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
lint (LintOptions _ ignored dfiles) = mapM_ (lintDockerfile ignored) dfiles

checkAst :: (RuleCheck -> Bool) -> Either ParseError Dockerfile -> IO ()
checkAst checkFilter ast =
    case ast of
        Left err -> putStrLn (formatError err) >> exitFailure
        Right dockerfile -> printChecks $ filter checkFilter $ analyzeAll dockerfile

analyzeAll :: Dockerfile -> [RuleCheck]
analyzeAll = analyze rules

-- Helper to analyze AST quickly in GHCI
analyzeEither :: Either t Dockerfile -> [RuleCheck]
analyzeEither (Left err) = []
analyzeEither (Right dockerfile) = analyzeAll dockerfile

die :: IO a
die = exitWith (ExitFailure 1)
