module Hadolint.Formatter.TTY
    ( printResult
    , formatError
    ) where

import Hadolint.Formatter.Format
import Hadolint.Rules
import Language.Docker.Syntax
import Text.Parsec.Error
       (ParseError, errorMessages, errorPos, showErrorMessages)
import Text.Parsec.Pos

formatErrors :: Functor t => t ParseError -> t String
formatErrors = fmap formatError

formatError :: ParseError -> String
formatError err = posPart ++ stripNewlines (formatErrorReason err)
  where
    pos = errorPos err
    posPart = sourceName pos ++ ":" ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)

formatChecks :: Functor t => t RuleCheck -> t String
formatChecks = fmap formatCheck
  where
    formatCheck (RuleCheck metadata source linenumber _) =
        formatPos source linenumber ++ code metadata ++ " " ++ message metadata

formatPos :: Filename -> Linenumber -> String
formatPos source linenumber =
    if linenumber >= 0
        then source ++ ":" ++ show linenumber ++ " "
        else source ++ " "

printResult :: Result -> IO ()
printResult (Result errors checks) = printErrors >> printChecks
  where
    printErrors = mapM_ putStrLn (formatErrors errors)
    printChecks = mapM_ putStrLn (formatChecks checks)
