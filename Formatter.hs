module Formatter (formatCheck) where

import Control.Monad
import Rules
import Syntax
import Analyzer

formatCheck :: Check -> String
formatCheck (DockerfileCheck rule _) = "[" ++ (name rule) ++ "] " ++ (message rule)
formatCheck (InstructionCheck rule pos) = (formatPos pos) ++ " [" ++ (name rule) ++ "] " ++ (message rule)

formatPos :: InstructionPos -> String
formatPos (InstructionPos _ linenumber) = "L" ++ (show linenumber)
