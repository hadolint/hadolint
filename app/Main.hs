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
data LintOptions = LintOptions { json :: Bool
                               , codeClimate :: Bool
                               , ignoreRules :: [IgnoreRule]
                               , dockerfile :: String
                               }


ignoreFilter :: [IgnoreRule] -> Check -> Bool
ignoreFilter ignoredRules (Check (Metadata code _ _) _ _ _) = code `notElem` ignoredRules


printChecks :: [Check] -> IO ()
printChecks checks = do
    mapM_ (putStrLn . formatCheck) $ sort checks
    if null checks then exit else die

parseOptions :: Parser LintOptions
parseOptions = LintOptions
    <$> switch (long "json" <> help "Format output as JSON")
    <*> switch (long "code-climate" <> help "Format output as CodeClimate compatible JSON")
    <*> many (strOption (long "ignore" <> help "Ignore rule" <> metavar "RULECODE"))
    <*> argument str (metavar "DOCKERFILE")

main :: IO ()
main = execParser opts >>= lint
    where
        opts = info (helper <*> parseOptions)
          ( fullDesc
         <> progDesc "Lint Dockerfile for errors and best practices"
         <> header "hadolint - Dockerfile Linter written in Haskell" )

lint :: LintOptions -> IO ()
lint (LintOptions _ _ ignoreRules dockerfile) = do
   ast <- parseFile dockerfile
   checkAst (ignoreFilter ignoreRules) ast

checkAst :: (Check -> Bool) -> Either ParseError Dockerfile -> IO ()
checkAst checkFilter ast = case ast of
    Left err         -> print err >> exit
    Right dockerfile -> printChecks $ filter checkFilter $ analyzeAll dockerfile

analyzeAll = analyze rules

-- Helper to analyze AST quickly in GHCI
analyzeEither (Left err) = []
analyzeEither (Right dockerfile)  = analyzeAll dockerfile

usage   = putStrLn "Usage: hadolint [-vhi] <file>"
version = putStrLn "Haskell Dockerfile Linter v1.0"
exit    = exitSuccess
die     = exitWith (ExitFailure 1)
