{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import Analyzer
import Formatter
import Rules
import Syntax

import Haste
import Haste.Foreign

instance ToAny Check where
    toAny (DockerfileCheck rule _)    = toObject [("message", toAny (message rule))
                                                 ,("name", toAny (name rule))]
    toAny (InstructionCheck rule pos) = toObject [("message", toAny (message rule))
                                                 ,("name", toAny (name rule))
                                                 ,("linenumber", toAny pos)]

instance ToAny InstructionPos where
    toAny (InstructionPos _ linenumber) = toAny linenumber

analyzeString :: String -> IO [Check]
analyzeString str = return $ failedChecks checks
    where ast = parseString str
          analyzeEither (Left err) = []
          analyzeEither (Right d) = analyze d
          checks = analyzeEither ast

main = do
    export "analyzeString" analyzeString
