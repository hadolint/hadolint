module Hadolint.Formatter.TTY
    ( formatError
    , formatChecks
    , printResults
    ) where

import Hadolint.Formatter.Format
import Hadolint.Rules
import Language.Docker.Syntax
import Text.Parsec.Error
       (ParseError, errorMessages, errorPos, showErrorMessages)
import Text.Parsec.Pos

formatError :: ParseError -> String
formatError err = posPart ++ formatErrorReason err
  where
    pos = errorPos err
    posPart = sourceName pos ++ ":" ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)

formatChecks :: [RuleCheck] -> [String]
formatChecks = map formatCheck
  where
    formatCheck (RuleCheck metadata source linenumber _) =
        formatPos source linenumber ++ code metadata ++ " " ++ message metadata

formatPos :: Filename -> Linenumber -> String
formatPos source linenumber =
    if linenumber >= 0
        then source ++ ":" ++ show linenumber ++ " "
        else source ++ " "

printResults :: Either ParseError [RuleCheck] -> IO ()
printResults = either printError printChecks
  where
    printError = putStrLn . formatError
    printChecks checks = mapM_ putStrLn (formatChecks checks)
