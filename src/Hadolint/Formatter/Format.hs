module Hadolint.Formatter.Format
    ( severityText
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
import Text.Megaparsec.Error (ParseError)

data Result t e = Result
    { errors :: Seq (ParseError t e)
    , checks :: Seq RuleCheck
    } deriving (Eq)

instance Semigroup (Result t e) where
    (Result e1 c1) <> (Result e2 c2) = Result (e1 <> e2) (c1 <> c2)

instance Monoid (Result t e) where
    mappend = (<>)
    mempty = Result mempty mempty

toResult :: Either (ParseError t e) [RuleCheck] -> Result t e
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

stripNewlines :: String -> String
stripNewlines =
    map
        (\c ->
             if c == '\n'
                 then ' '
                 else c)
