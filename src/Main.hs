module Main where

import Parser
import Rules
import Formatter

import System.Environment (getArgs)
import System.Exit hiding (die)

printChecks :: [Check] -> IO ()
printChecks = mapM_ (putStrLn . formatCheck)

main :: IO ()
main = getArgs >>= parse

parse []     = usage   >> die
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse [file] = do
    ast <- parseFile file
    case ast of
        Left err         -> print err >> exit
        Right dockerfile -> printChecks $ analyzeAll dockerfile

analyzeAll = analyze allRules
analyzeBestPractices = analyze bestPracticeRules

-- Helper to analyze AST quickly in GHCI
analyzeEither (Left err) = []
analyzeEither (Right dockerfile)  = analyzeAll dockerfile

usage   = putStrLn "Usage: hadolint [-vh] <file>"
version = putStrLn "Haskell Dockerfile Linter v0.1"
exit    = exitSuccess
die     = exitWith (ExitFailure 1)
