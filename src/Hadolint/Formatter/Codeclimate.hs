{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hadolint.Formatter.Codeclimate
    ( printResult
    , formatResult
    ) where

import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy as B
import Data.DList (DList)
import Data.Monoid ((<>))
import GHC.Generics
import Hadolint.Formatter.Format (Result(..), formatErrorReason)
import Hadolint.Rules (Metadata(..), RuleCheck(..))
import ShellCheck.Interface
import Text.Parsec.Error (            errorPos)
import Text.Parsec.Pos

data Issue = Issue
    { checkName :: String
    , description :: String
    , location :: Location
    , impact :: String
    }

data Location
    = LocLine String
              Int
    | LocPos String
             Pos

instance ToJSON Location where
    toJSON (LocLine path l) = object ["path" .= path, "lines" .= object ["begin" .= l, "end" .= l]]
    toJSON (LocPos path pos) =
        object ["path" .= path, "positions" .= object ["begin" .= pos, "end" .= pos]]

data Pos = Pos
    { line :: Int
    , column :: Int
    } deriving (Generic)

instance ToJSON Pos

instance ToJSON Issue where
    toJSON Issue {..} =
        object
            [ "type" .= ("issue" :: String)
            , "check_name" .= checkName
            , "description" .= description
            , "categories" .= (["Bug Risk"] :: [String])
            , "location" .= location
            , "severity" .= impact
            ]

errorToIssue err =
    Issue
    { checkName = "DL1000"
    , description = formatErrorReason err
    , location = LocPos (sourceName pos) (Pos (sourceLine pos) (sourceColumn pos))
    , impact = severityText ErrorC
    }
  where
    pos = errorPos err

checkToIssue RuleCheck {..} =
    Issue
    { checkName = code metadata
    , description = message metadata
    , location = LocLine filename linenumber
    , impact = severityText (severity metadata)
    }

severityText :: Severity -> String
severityText severity =
    case severity of
        ErrorC -> "blocker"
        WarningC -> "major"
        InfoC -> "info"
        StyleC -> "minor"

formatResult :: Result -> DList Issue
formatResult (Result errors checks) = allIssues
  where
    allIssues = errorMessages <> checkMessages
    errorMessages = fmap errorToIssue errors
    checkMessages = fmap checkToIssue checks

printResult :: Result -> IO ()
printResult result = mapM_ output (formatResult result)
  where
    output value = do
        B.putStr (encode value)
        B.putStr (B.singleton 0x00)
