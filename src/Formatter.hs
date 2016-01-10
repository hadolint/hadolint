module Formatter (formatCheck) where

import Control.Monad
import Rules
import Syntax

formatCheck :: Check -> String
formatCheck (Check metadata linenumber _) = formatPos linenumber ++ code metadata ++ " " ++ message metadata

formatPos :: Linenumber -> String
formatPos linenumber = if linenumber >= 0
                       then "L" ++ show linenumber ++ " "
                       else ""
