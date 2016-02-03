module Main where

import Parser
import Rules
import Formatter
import Syntax

import System.Environment (getArgs)
import System.Exit hiding (die)
import Data.List (sort)
import Text.Parsec (ParseError)
import Options.Applicative hiding (ParseError)

data LintOptions = LintOptions { readStdin :: Bool
                               , json :: Bool
                               , codeClimate :: Bool
                               }


printChecks :: [Check] -> IO ()
printChecks checks = do
    mapM_ (putStrLn . formatCheck) $ sort checks
    exitWith $ ExitFailure $ length checks

parseOptions :: Parser LintOptions
parseOptions = LintOptions
    <$> switch (long "read-stdin" <> help "Read file contents from stdin")
    <*> switch (long "json" <> help "Format output as JSON")
    <*> switch (long "code-climate" <> help "Format output as CodeClimate compatible JSON")

main :: IO ()
main = execParser opts >>= lint
    where
        opts = info (helper <*> parseOptions)
          ( fullDesc
         <> progDesc "Lint Dockerfile for errors and best practices"
         <> header "hadolint - Dockerfile Linter written in Haskell" )

lint :: LintOptions -> IO ()
lint LintOptions(True ) = do
    content <- getContents
    length content `seq` return ()
    if not (null content)
    then checkAst $ parseString content
    else usage >> die
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse [file] = parseFile file >>= checkAst

checkAst :: Either ParseError Dockerfile -> IO ()
checkAst ast = case ast of
    Left err         -> print err >> exit
    Right dockerfile -> printChecks $ analyzeAll dockerfile

analyzeAll = analyze rules

-- Helper to analyze AST quickly in GHCI
analyzeEither (Left err) = []
analyzeEither (Right dockerfile)  = analyzeAll dockerfile

usage   = putStrLn "Usage: hadolint [-vhi] <file>"
version = putStrLn "Haskell Dockerfile Linter v0.1"
exit    = exitSuccess
die     = exitWith (ExitFailure 1)
