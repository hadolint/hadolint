module Hadolint.Formatter
    ( formatCheck
    , formatError
    ) where

import Control.Monad
import Data.Char (isSpace)
import Hadolint.Rules
import Language.Docker.Syntax
import Text.Parsec.Error
       (Message, ParseError, errorMessages, errorPos, messageString,
        showErrorMessages)
import Text.Parsec.Pos

formatError :: ParseError -> String
formatError err = posPart ++ stripNewlines msgPart
  where
    pos = errorPos err
    posPart = sourceName pos ++ ":" ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
    msgPart =
        showErrorMessages
            "or"
            "unknown parse error"
            "expecting"
            "unexpected"
            "end of input"
            (errorMessages err)
    stripNewlines =
        map
            (\c ->
                 if c == '\n'
                     then ' '
                     else c)

formatCheck :: RuleCheck -> String
formatCheck (RuleCheck metadata source linenumber _) =
    formatPos source linenumber ++ code metadata ++ " " ++ message metadata

formatPos :: Filename -> Linenumber -> String
formatPos source linenumber =
    if linenumber >= 0
        then source ++ ":" ++ show linenumber ++ " "
        else source ++ " "
