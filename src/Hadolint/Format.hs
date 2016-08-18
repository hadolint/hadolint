{-# LANGUAGE OverloadedStrings #-}
module Hadolint.Format where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import ShellCheck.Interface

import Hadolint.Rules
import Hadolint.Syntax

instance Show Check where
    show (Check metadata source linenumber _) = formatPos source linenumber ++ code metadata ++ " " ++ message metadata

formatPos :: Filename -> Linenumber -> String
formatPos source linenumber = if linenumber >= 0
                              then source ++ ":" ++ show linenumber ++ " "
                              else source ++ " "

-- | Code climate impact based of the severity
impact :: Severity -> String
impact ErrorC = "critical"
impact WarningC = "normal"
impact InfoC = "info"
impact StyleC = "info"

-- | Code climate categories for rule code
-- https://github.com/codeclimate/spec/blob/master/SPEC.md#categories
categorize :: String -> String
categorize "DL3004" = "Security"
categorize _ = "Compatibility"

-- | Take code and return wiki url
wikiUrl :: String -> String
wikiUrl code = "https://github.com/lukasmartinelli/hadolint/wiki/" ++ code
-- | Emit Code Climate compatible JSON structure of check
-- https://github.com/codeclimate/spec/blob/master/SPEC.md
instance ToJSON Check where
    toJSON (Check m fname lineno success ) = object [ "type" .= T.pack "issue"
                      , "check_name" .= T.pack (code m)
                      , "description" .= T.pack (message m)
                      , "content" .= object [ "body" .= wikiUrl (code m) ]
                      , "categories" .= [categorize (code m)]
                      , "severity" .= impact (severity m)
                      , "location" .= object [ "path" .= fname
                                             , "lines" .= object ["begin" .= lineno , "end" .= lineno]
                                             ]
                      ]
