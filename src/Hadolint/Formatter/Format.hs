module Hadolint.Formatter.Format
    ( formatErrorReason
    , severityText
    , stripNewlines
    , Result(..)
    , toResult
    ) where

import Data.List (sort)
import Data.Monoid (Monoid)
import Data.Semigroup
import Data.Sequence (Seq, fromList, singleton)
import Hadolint.Rules
import ShellCheck.Interface
import Text.Parsec.Error
       (ParseError, errorMessages, showErrorMessages)

data Result = Result
    { errors :: Seq ParseError
    , checks :: Seq RuleCheck
    } deriving (Eq)

instance Semigroup Result where
    (Result e1 c1) <> (Result e2 c2) = Result (e1 <> e2) (c1 <> c2)

instance Monoid Result where
    mappend = (<>)
    mempty = Result mempty mempty

toResult :: Either ParseError [RuleCheck] -> Result
toResult res =
    case res of
        Left err -> Result (singleton err) mempty
        Right c -> Result mempty (fromList (sort c))

severityText :: Severity -> String
severityText s =
    case s of
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
