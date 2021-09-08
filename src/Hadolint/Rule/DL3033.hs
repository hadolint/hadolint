module Hadolint.Rule.DL3033 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

rule :: Rule Shell.ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3033"
    severity = DLWarningC
    message = "Specify version with `yum install -y <package>-<version>`."

    check (Run (RunArgs args _)) =
      foldArguments (all packageVersionFixed . yumPackages) args
        && foldArguments (all moduleVersionFixed . yumModules) args
    check _ = True
{-# INLINEABLE rule #-}

yumPackages :: Shell.ParsedShell -> [Text.Text]
yumPackages args =
  [ arg
    | cmd <- Shell.presentCommands args,
      not (Shell.cmdHasArgs "yum" ["module"] cmd),
      arg <- installFilter cmd
  ]

packageVersionFixed :: Text.Text -> Bool
packageVersionFixed package =
  "-" `Text.isInfixOf` package || ".rpm" `Text.isSuffixOf` package

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
