module Hadolint.Rule.DL3013 (rule) where

import Data.List (isInfixOf)
import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax


rule :: Rule ParsedShell
rule = dl3013 <> onbuild dl3013
{-# INLINEABLE rule #-}

dl3013 :: Rule ParsedShell
dl3013 = simpleRule code severity message check
  where
    code = "DL3013"
    severity = DLWarningC
    message =
      "Pin versions in pip. Instead of `pip install <package>` use `pip install \
      \<package>==<version>` or `pip install --requirement <requirements file>`."
    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotToPinVersion) args
    check _ = True

    forgotToPinVersion cmd =
      isPipInstall cmd
        && not (hasBuildConstraint cmd)
        && not (all versionFixed (packages cmd))

    -- Check if the command is a pip* install command, and that specific packages are being listed
    isPipInstall cmd =
      ( Shell.isPipInstall cmd
          && not (hasBuildConstraint cmd)
          && not (all versionFixed (packages cmd))
      )
        && not (requirementInstall cmd)

    -- If the user is installing requirements from a file or just the local module, then we are not interested
    -- in running this rule
    requirementInstall cmd =
      ["--requirement"] `isInfixOf` Shell.getArgs cmd
        || ["-r"] `isInfixOf` Shell.getArgs cmd
        || ["."] `isInfixOf` Shell.getArgs cmd

    hasBuildConstraint cmd = Shell.hasFlag "constraint" cmd || Shell.hasFlag "c" cmd
    versionFixed package = hasVersionSymbol package
      || isVersionedVcs package
      || isLocalPackage package
      || isNoVcsPathSource package
    isVersionedVcs package = isVcs package
      && "@" `Text.isInfixOf` package
    versionSymbols = ["==", ">=", "<=", ">", "<", "!=", "~=", "==="]
    hasVersionSymbol package = or [s `Text.isInfixOf` package | s <- versionSymbols]
    localPackageFileExtensions = [".whl", ".tar.gz"]
    isLocalPackage package = or [s `Text.isSuffixOf` package | s <- localPackageFileExtensions]
    isNoVcsPathSource package = not (isVcs package) && "/" `Text.isInfixOf` package
    isVcs package = any (`Text.isPrefixOf` package) vcsSchemes
{-# INLINEABLE dl3013 #-}

packages :: Shell.Command -> [Text.Text]
packages cmd =
  stripInstallPrefix $
    Shell.getArgsNoFlags $
      Shell.dropFlagArg
        [ "abi",
          "b",
          "build",
          "e",
          "editable",
          "extra-index-url",
          "f",
          "find-links",
          "i",
          "index-url",
          "implementation",
          "no-binary",
          "only-binary",
          "platform",
          "prefix",
          "progress-bar",
          "proxy",
          "python-version",
          "root",
          "src",
          "t",
          "target",
          "trusted-host",
          "upgrade-strategy"
        ]
        cmd

-- Supported schemes vcs[+protocol] are found here:
-- https://pip.pypa.io/en/stable/topics/vcs-support/
vcsSchemes :: [Text.Text]
vcsSchemes =
  [
    "git+file",
    "git+https",
    "git+ssh",
    "git+http",
    "git+git",
    "git",
    "hg+file",
    "hg+http",
    "hg+https",
    "hg+ssh",
    "hg+static-http",
    "svn",
    "svn+svn",
    "svn+http",
    "svn+https",
    "svn+ssh",
    "bzr+http",
    "bzr+https",
    "bzr+ssh",
    "bzr+sftp",
    "bzr+ftp",
    "bzr+lp"
  ]

stripInstallPrefix :: [Text.Text] -> [Text.Text]
stripInstallPrefix cmd = dropWhile (== "install") (dropWhile (/= "install") cmd)
