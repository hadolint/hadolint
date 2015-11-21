module Main where

import Parser
import Analyzer
import Formatter

import System.Environment (getArgs)
import System.Exit hiding (die)

printFailedChecks :: [Check] -> IO ()
printFailedChecks checks = mapM_ putStrLn $ map formatCheck $ failedChecks checks

main :: IO ()
main = getArgs >>= parse

parse []     = usage   >> die
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse [file] = do
    ast <- parseFile file
    case ast of
        Left err -> print err >> exit
        Right d  -> printFailedChecks $ analyze d

-- Helper to analyze AST quickly in GHCI
analyzeEither (Left err) = []
analyzeEither (Right d)  = analyze d

usage   = putStrLn "Usage: hadolint [-vh] <file>"
version = putStrLn "Haskell Dockerfile Linter v0.1"
exit    = exitSuccess
die     = exitWith (ExitFailure 1)
