module Hadolint.Formatter.Format
    ( formatErrorReason
    , severityText
    , stripNewlines
    , Result(..)
    , toResult
    ) where

import Data.DList (DList, fromList, singleton)
import Data.List (sort)
import Data.Monoid
import Hadolint.Rules
import Language.Docker.Syntax
import ShellCheck.Interface
import Text.Parsec.Error
       (ParseError, errorMessages, errorPos, showErrorMessages)

data Result = Result
    { errors :: DList ParseError
    , checks :: DList RuleCheck
    } deriving (Eq)

instance Monoid Result where
    mempty = Result mempty mempty
    mappend (Result e1 c1) (Result e2 c2) = Result (e1 <> e2) (c1 <> c2)

toResult :: Either ParseError [RuleCheck] -> Result
toResult res =
    case res of
        Left err -> Result (singleton err) mempty
        Right c -> Result mempty (fromList (sort c))

severityText :: Severity -> String
severityText severity =
    case severity of
        ErrorC -> "error"
        WarningC -> "warning"
        InfoC -> "info"
        StyleC -> "style"

formatErrorReason :: ParseError -> String
formatErrorReason err =
    showErrorMessages
        "or"
        "unknown parse error"
        "expecting"
        "unexpected"
        "end of input"
        (errorMessages err)

stripNewlines :: String -> String
stripNewlines =
    map
        (\c ->
             if c == '\n'
                 then ' '
                 else c)
