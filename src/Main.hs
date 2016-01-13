module Main where

import Parser
import Rules
import Formatter

import System.Environment (getArgs)
import System.Exit hiding (die)
import Data.List (sort)

printChecks :: [Check] -> IO ()
printChecks = mapM_ (putStrLn . formatCheck) . sort

main :: IO ()
main = getArgs >>= parse

parse [] = usage >> die
parse ["-i"] = do
    content <- getContents
    length content `seq` return ()
    if not (null content)
    then case parseString content of
            Left err         -> print err >> exit
            Right dockerfile -> printChecks $ analyzeAll dockerfile
    else usage >> die
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse [file] = do
    ast <- parseFile file
    case ast of
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
