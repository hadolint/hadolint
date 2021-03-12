module Hadolint.Rule.DL3016 (rule) where

import Data.List (isPrefixOf)
import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax (Instruction (..), RunArgs (..))

-- | Rule for pinning NPM packages to version, tag, or commit
--  supported formats by Hadolint
--    npm install (with no args, in package dir)
--    npm install [<@scope>/]<name>
--    npm install [<@scope>/]<name>@<tag>
--    npm install [<@scope>/]<name>@<version>
--    npm install git[+http|+https]://<git-host>/<git-user>/<repo-name>[#<commit>|#semver:<semver>]
--    npm install git+ssh://<git-host>:<git-user>/<repo-name>[#<commit>|#semver:<semver>]
rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3016"
    severity = DLWarningC
    message =
      "Pin versions in npm. Instead of `npm install <package>` use `npm install \
      \<package>@<version>`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotToPinVersion) args
    check _ = True

    forgotToPinVersion cmd =
      isNpmInstall cmd && installIsFirst cmd && not (all versionFixed (packages cmd))

    isNpmInstall = Shell.cmdHasArgs "npm" ["install"]
    installIsFirst cmd = ["install"] `isPrefixOf` Shell.getArgsNoFlags cmd
    packages cmd = stripInstallPrefix (Shell.getArgsNoFlags cmd)

    versionFixed package
      | hasGitPrefix package = isVersionedGit package
      | hasTarballSuffix package = True
      | isFolder package = True
      | otherwise = hasVersionSymbol package

    gitPrefixes = ["git://", "git+ssh://", "git+http://", "git+https://"]
    pathPrefixes = ["/", "./", "../", "~/"]
    tarballSuffixes = [".tar", ".tar.gz", ".tgz"]

    hasGitPrefix package = or [p `Text.isPrefixOf` package | p <- gitPrefixes]
    hasTarballSuffix package = or [p `Text.isSuffixOf` package | p <- tarballSuffixes]
    isFolder package = or [p `Text.isPrefixOf` package | p <- pathPrefixes]
    isVersionedGit package = "#" `Text.isInfixOf` package

    hasVersionSymbol package = "@" `Text.isInfixOf` dropScope package
      where
        dropScope pkg =
          if "@" `Text.isPrefixOf` pkg
            then Text.dropWhile ('/' <) pkg
            else pkg
{-# INLINEABLE rule #-}

stripInstallPrefix :: [Text.Text] -> [Text.Text]
stripInstallPrefix cmd = dropWhile (== "install") (dropWhile (/= "install") cmd)
