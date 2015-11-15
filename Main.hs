module Main where

import Parser
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    ast <- parseFile (args !! 0)
    case ast of
        Left err  -> print err
        Right r   -> print r
