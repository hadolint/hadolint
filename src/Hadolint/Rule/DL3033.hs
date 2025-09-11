module Hadolint.Rule.DL3033 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax
import Data.Char (isDigit, isAsciiUpper, isAsciiLower)


rule :: Rule Shell.ParsedShell
rule = dl3033 <> onbuild dl3033
{-# INLINEABLE rule #-}

dl3033 :: Rule Shell.ParsedShell
dl3033 = simpleRule code severity message check
  where
    code = "DL3033"
    severity = DLWarningC
    message = "Specify version with `yum install -y <package>-<version>`."

    check (Run (RunArgs args _)) =
      foldArguments (all packageVersionFixed . yumPackages) args
        && foldArguments (all moduleVersionFixed . yumModules) args
    check _ = True
{-# INLINEABLE dl3033 #-}

yumPackages :: Shell.ParsedShell -> [Text.Text]
yumPackages args =
  [ arg
    | cmd <- Shell.presentCommands args,
      not (Shell.cmdHasArgs "yum" ["module"] cmd),
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
isVersionChar c =
  isDigit c
    || isAsciiUpper c
    || isAsciiLower c
    || c `elem` ['.', '~', '^', '_', ':', '+']

yumModules :: Shell.ParsedShell -> [Text.Text]
yumModules args =
  [ arg
    | cmd <- Shell.presentCommands args,
      Shell.cmdHasArgs "yum" ["module"] cmd,
      arg <- installFilter cmd
  ]

moduleVersionFixed :: Text.Text -> Bool
moduleVersionFixed = Text.isInfixOf ":"

installFilter :: Shell.Command -> [Text.Text]
installFilter cmd =
  [ arg
    | Shell.cmdHasArgs "yum" ["install"] cmd,
      arg <- Shell.getArgsNoFlags cmd,
      arg /= "install",
      arg /= "module"
  ]
