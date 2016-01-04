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
    toAny (Check rule result) = toObject [("message", toAny (message rule))
                                         ,("name", toAny (name rule))
                                         ,("linenumber", toAny result)]

instance ToAny RuleResult where
    toAny (RuleResult linenumber _) = toAny linenumber

analyzeString :: String -> IO [Check]
analyzeString str = return checks
    where ast = parseString str
          analyzeEither (Left err) = []
          analyzeEither (Right d) = analyzeAll d
          checks = analyzeEither ast

analyzeAll = analyze allRules
hayabusa = length $ shellcheck "echo $1"

main = do
    export "analyzeString" analyzeString
    export "hayabusa" hayabusa
