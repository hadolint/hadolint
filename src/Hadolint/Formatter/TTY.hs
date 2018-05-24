{-# LANGUAGE NamedFieldPuns #-}

module Hadolint.Formatter.TTY
    ( printResult
    , formatError
    ) where

import Hadolint.Formatter.Format
import Hadolint.Rules
import Language.Docker.Syntax
import Text.Parsec.Error (ParseError, errorPos)
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
    formatCheck (RuleCheck meta source line _) =
        formatPos source line ++ code meta ++ " " ++ message meta

formatPos :: Filename -> Linenumber -> String
formatPos source line = source ++ ":" ++ show line ++ " "

printResult :: Result -> IO ()
printResult Result {errors, checks} = printErrors >> printChecks
  where
    printErrors = mapM_ putStrLn (formatErrors errors)
    printChecks = mapM_ putStrLn (formatChecks checks)
