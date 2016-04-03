module Main where

import Parser
import Rules
import Formatter
import Syntax

import System.Environment (getArgs)
import System.Exit hiding (die)
import Data.List (sort)
import Text.Parsec (ParseError)

printChecks :: [Check] -> IO ()
printChecks checks = do
    mapM_ (putStrLn . formatCheck) $ sort checks
    if null checks then exit else die

main :: IO ()
main = getArgs >>= parse

parse [] = usage >> die
parse ["-i"] = do
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
    Left err         -> print err >> die
    Right dockerfile -> printChecks $ analyzeAll dockerfile

analyzeAll = analyze rules

-- Helper to analyze AST quickly in GHCI
analyzeEither (Left err) = []
analyzeEither (Right dockerfile)  = analyzeAll dockerfile

usage   = putStrLn "Usage: hadolint [-vhi] <file>"
version = putStrLn "Haskell Dockerfile Linter v1.0"
exit    = exitSuccess
die     = exitWith (ExitFailure 1)
