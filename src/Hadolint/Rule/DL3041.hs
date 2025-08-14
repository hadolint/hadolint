module Hadolint.Rule.DL3041 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax
import Data.Char (isDigit, isAsciiUpper, isAsciiLower)

rule :: Rule Shell.ParsedShell
rule = dl3041 <> onbuild dl3041
{-# INLINEABLE rule #-}

dl3041 :: Rule Shell.ParsedShell
dl3041 = simpleRule code severity message check
  where
    code = "DL3041"
    severity = DLWarningC
    message = "Specify version with `dnf install -y <package>-<version>`."

    check (Run (RunArgs args _)) =
      foldArguments (all packageVersionFixed . dnfPackages) args
        && foldArguments (all moduleVersionFixed . dnfModules) args
    check _ = True
{-# INLINEABLE dl3041 #-}

dnfCmds :: [Text.Text]
dnfCmds = ["dnf", "microdnf"]

dnfPackages :: Shell.ParsedShell -> [Text.Text]
dnfPackages args =
    [ arg
      | cmd <- Shell.presentCommands args,
        not (Shell.cmdsHaveArgs dnfCmds ["module"] cmd),
        arg <- installFilter cmd
    ]

packageVersionFixed :: Text.Text -> Bool
packageVersionFixed package
  | length parts <= 1 = False  -- No dashes, definitively no version
  | ".rpm" `Text.isSuffixOf` package = True  -- rpm files always have a version
  | otherwise = isVersionLike $ drop 1 parts
  where
    parts = Text.splitOn "-" package

isVersionLike :: [Text.Text] -> Bool
isVersionLike parts =
  case parts of
    [] -> False  -- No parts after splitting by hyphen
    _ -> all partIsValid parts && any partStartsWithDigit parts
  where
    partIsValid part = Text.all isVersionChar part
    partStartsWithDigit part = case Text.uncons part of
                                 Just (c, _) -> isDigit c
                                 Nothing -> False -- Empty Text

isVersionChar :: Char -> Bool
isVersionChar c = isDigit c || isAsciiUpper c || isAsciiLower c || c `elem` ['.', '~', '^', '_']

dnfModules :: Shell.ParsedShell -> [Text.Text]
dnfModules args =
  [ arg
    | cmd <- Shell.presentCommands args,
      Shell.cmdsHaveArgs dnfCmds ["module"] cmd,
      arg <- installFilter cmd
  ]

moduleVersionFixed :: Text.Text -> Bool
moduleVersionFixed = Text.isInfixOf ":"

installFilter :: Shell.Command -> [Text.Text]
installFilter cmd =
  [ arg
    | Shell.cmdsHaveArgs dnfCmds ["install"] cmd,
      arg <- Shell.getArgsNoFlags cmd,
      arg /= "install",
      arg /= "module"
  ]
