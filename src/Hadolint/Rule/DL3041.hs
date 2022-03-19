module Hadolint.Rule.DL3041 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax


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
packageVersionFixed package =
  "-" `Text.isInfixOf` package || ".rpm" `Text.isSuffixOf` package

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
