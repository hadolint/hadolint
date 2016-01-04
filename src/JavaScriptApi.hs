{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import Formatter
import Rules
import Syntax
import Bash

import Haste
import Haste.Foreign

instance ToAny Check where
    toAny (rule, result) = toObject [("message", toAny (message rule))
                                    ,("name", toAny (name rule))
                                    ,("linenumber", toAny result)]

instance ToAny RuleResult where
    toAny (linenumber, _) = toAny linenumber

analyzeString :: String -> IO [Check]
analyzeString str = return checks
    where ast = parseString str
          analyzeEither (Left err) = []
          analyzeEither (Right d) = analyze d
          checks = analyzeEither ast

hayabusa = shellcheck "echo $1"
main = export "analyzeString" analyzeString
