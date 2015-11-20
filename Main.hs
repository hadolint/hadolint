module Main where

import Parser
import Analyzer

import System.Environment (getArgs)
import System.Exit hiding (die)

printFailedChecks :: [String] -> IO ()
printFailedChecks str = do
    putStrLn "L1-12 [NoDefaultCmd] Specify a default CMD"
    putStrLn "L1-12 [WgetAndCurlUsed] Either use curl or wget but not both"
    putStrLn "L10 [NoSudo] Do not use sudo inside Dockerfile. "

main :: IO ()
main = getArgs >>= parse

parse []     = usage   >> die
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse [file] = do
    ast <- parseFile file
    case ast of
        Left err  -> print err >> exit
        Right r   -> printFailedChecks $ analyze r

usage   = putStrLn "Usage: hadolint [-vh] <file>"
version = putStrLn "Haskell Dockerfile Linter v0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
