module Hadolint.Rule.DL3062 (rule) where

import Data.Text qualified as Text
import Hadolint.Rule
import Hadolint.Shell qualified as Shell
import Language.Docker.Syntax

rule :: Rule Shell.ParsedShell
rule = dl3062 <> onbuild dl3062
{-# INLINEABLE rule #-}

dl3062 :: Rule Shell.ParsedShell
dl3062 = simpleRule code severity message check
  where
    code = "DL3062"
    severity = DLWarningC
    message = "Pin versions in go. Instead of `go install <package>` use `go install <package>@<version>`"

    check (Run (RunArgs args _)) = foldArguments (all checkGoPackageVersion . getGoPackage) args
    check _ = True
{-# INLINEABLE dl3062 #-}

goCommands :: [Text.Text]
goCommands = ["install", "get", "run"]

getGoPackage :: Shell.ParsedShell -> [Text.Text]
getGoPackage args =
  [ arg
    | cmd <- Shell.presentCommands args,
      Shell.cmdsHaveArgs ["go"] goCommands cmd,
      (idx, arg) <- enum $ Shell.getArgsNoFlags cmd,
      arg `notElem` goCommands && idx <= 1  -- everything after the package name is an argument to the go program
  ]
  where
    enum :: [Text.Text] -> [(Integer, Text.Text)]
    enum = zip [0..]

hasVersionSymbol :: Text.Text -> Bool
hasVersionSymbol package = "@" `Text.isInfixOf` package

isTagsVersion :: Text.Text -> Bool
isTagsVersion package =
  or [("@" <> tag) `Text.isSuffixOf` package | tag <- ["latest", "none"]]

isLocalPath :: Text.Text -> Bool
isLocalPath p = p == "."
  || "/" `Text.isPrefixOf` p
  || "." `Text.isPrefixOf` p

checkGoPackageVersion :: Text.Text -> Bool
checkGoPackageVersion package =
  isLocalPath package
    || hasVersionSymbol package
    && not (isTagsVersion package)
