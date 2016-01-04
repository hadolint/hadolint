module Formatter (formatCheck) where

import Control.Monad
import Rules
import Syntax

formatCheck :: Check -> String
formatCheck (Check rule result) = formatPos result ++ "[" ++ name rule ++ "] " ++ message rule

formatPos :: RuleResult -> String
formatPos (RuleResult linenumber _) = if linenumber >= 0
                                      then "L" ++ show linenumber ++ " "
                                      else ""
