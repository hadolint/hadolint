module Hadolint.Rule.DL3016 (rule) where

import Data.List (isPrefixOf)
import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax (Instruction (..), RunArgs (..))


rule :: Rule ParsedShell
rule = dl3016 <> onbuild dl3016
{-# INLINEABLE rule #-}

-- | Rule for pinning NPM packages to version, tag, or commit
--  supported formats by Hadolint
--    npm install (with no args, in package dir)
--    npm install [<@scope>/]<name>
--    npm install [<@scope>/]<name>@<tag>
--    npm install [<@scope>/]<name>@<version>
--    npm install git[+http|+https]://<git-host>/<git-user>/<repo-name>[#<commit>|#semver:<semver>]
--    npm install git+ssh://<git-host>:<git-user>/<repo-name>[#<commit>|#semver:<semver>]
dl3016 :: Rule ParsedShell
dl3016 = simpleRule code severity message check
  where
    code = "DL3016"
    severity = DLWarningC
    message =
      "Pin versions in npm. Instead of `npm install <package>` use `npm install \
      \<package>@<version>`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotToPinVersion) args
    check _ = True
{-# INLINEABLE dl3016 #-}


forgotToPinVersion :: Shell.Command -> Bool
forgotToPinVersion cmd =
  isNpmInstall cmd && installIsFirst cmd && not (all versionFixed (packages cmd))

isNpmInstall :: Shell.Command -> Bool
isNpmInstall = Shell.cmdHasArgs "npm" ["install"]

installIsFirst :: Shell.Command -> Bool
installIsFirst cmd = ["install"] `isPrefixOf` Shell.getArgsNoFlags
  (Shell.dropFlagArg ignoreFlags cmd)

packages :: Shell.Command -> [Text.Text]
packages cmd = stripInstallPrefix
  (Shell.getArgsNoFlags (Shell.dropFlagArg ignoreFlags cmd))

versionFixed :: Text.Text -> Bool
versionFixed package
  | hasGitPrefix package = isVersionedGit package
  | hasTarballSuffix package = True
  | isFolder package = True
  | otherwise = hasVersionSymbol package

hasGitPrefix :: Text.Text -> Bool
hasGitPrefix package = or [p `Text.isPrefixOf` package | p <- gitPrefixes]

hasTarballSuffix :: Text.Text -> Bool
hasTarballSuffix package = or [p `Text.isSuffixOf` package | p <- tarballSuffixes]

isFolder :: Text.Text -> Bool
isFolder package = or [p `Text.isPrefixOf` package | p <- pathPrefixes]

isVersionedGit :: Text.Text -> Bool
isVersionedGit package = "#" `Text.isInfixOf` package

hasVersionSymbol :: Text.Text -> Bool
hasVersionSymbol package = "@" `Text.isInfixOf` dropScope package
  where
    dropScope pkg =
      if "@" `Text.isPrefixOf` pkg
        then Text.dropWhile ('/' <) pkg
        else pkg

stripInstallPrefix :: [Text.Text] -> [Text.Text]
stripInstallPrefix cmd = dropWhile (== "install") (dropWhile (/= "install") cmd)


ignoreFlags :: [Text.Text]
ignoreFlags = ["loglevel"]

gitPrefixes :: [Text.Text]
gitPrefixes = ["git://", "git+ssh://", "git+http://", "git+https://"]

pathPrefixes :: [Text.Text]
pathPrefixes = ["/", "./", "../", "~/"]

tarballSuffixes :: [Text.Text]
tarballSuffixes = [".tar", ".tar.gz", ".tgz"]
