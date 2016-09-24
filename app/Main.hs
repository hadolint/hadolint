module Main where

import Hadolint.Parser
import Hadolint.Rules
import Hadolint.Formatter
import Hadolint.Syntax

import System.Environment (getArgs)
import System.Exit hiding (die)
import Data.List (sort)
import Text.Parsec (ParseError)
import Control.Applicative
import Options.Applicative hiding (ParseError)

type IgnoreRule = String
data LintOptions = LintOptions { showVersion :: Bool
                               , ignoreRules :: [IgnoreRule]
                               , dockerfiles :: [String]
                               }

ignoreFilter :: [IgnoreRule] -> Check -> Bool
ignoreFilter ignoredRules (Check (Metadata code _ _) _ _ _) = code `notElem` ignoredRules

printChecks :: [Check] -> IO ()
printChecks checks = do
    mapM_ (putStrLn . formatCheck) $ sort checks
    if null checks then exitSuccess else die

parseOptions :: Parser LintOptions
parseOptions = LintOptions
    <$> switch (long "version" <> short 'v' <> help "Show version")
    <*> many (strOption (long "ignore" <> help "Ignore rule" <> metavar "RULECODE"))
    <*> many (argument str (metavar "DOCKERFILE..."))

main :: IO ()
main = execParser opts >>= lint
    where
        opts = info (helper <*> parseOptions)
          ( fullDesc
         <> progDesc "Lint Dockerfile for errors and best practices"
         <> header "hadolint - Dockerfile Linter written in Haskell" )

-- | Support UNIX convention of passing "-" instead of "/dev/stdin"
parseFilename :: String -> String
parseFilename "-" = "/dev/stdin"
parseFilename s = s

lintDockerfile :: [IgnoreRule] -> String -> IO ()
lintDockerfile ignoreRules dockerfile = do
    ast <- parseFile $ parseFilename dockerfile
    checkAst (ignoreFilter ignoreRules) ast

lint :: LintOptions -> IO ()
lint (LintOptions True _ _) = putStrLn "Haskell Dockerfile Linter v1.2.1" >> exitSuccess
lint (LintOptions _ _ []) = putStrLn "Please provide a Dockerfile" >> exitSuccess
lint (LintOptions _ ignored dfiles) = mapM_ (lintDockerfile ignored) dfiles

checkAst :: (Check -> Bool) -> Either ParseError Dockerfile -> IO ()
checkAst checkFilter ast = case ast of
    Left err         -> putStrLn (formatError err) >> exitSuccess
    Right dockerfile -> printChecks $ filter checkFilter $ analyzeAll dockerfile

analyzeAll = analyze rules

-- Helper to analyze AST quickly in GHCI
analyzeEither (Left err) = []
analyzeEither (Right dockerfile)  = analyzeAll dockerfile

die     = exitWith (ExitFailure 1)
