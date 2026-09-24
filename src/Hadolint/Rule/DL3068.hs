module Hadolint.Rule.DL3068 (rule) where

import Data.List (isPrefixOf)
import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax (Instruction (..), RunArgs (..))


rule :: Rule ParsedShell
rule = dl3068 <> onbuild dl3068
{-# INLINEABLE rule #-}

-- | Rule for pinning PECL packages to a version.
--  Supported package specifications (per the PEAR/pecl `install` command):
--    Abstract package : pecl install [channel/]<name>-<version>
--    Channel URI      : pecl install channel://<channel>/<name>-<version>
--    Release archive  : pecl install [path/]<name>-<version>.tgz  (local or http[s] URL)
--    Descriptor       : pecl install [path/]package.xml
--  A pinned version MUST be numeric (e.g. `redis-6.1.0`). Stability
--  keywords such as `-alpha`, `-beta` and `-stable` float and are therefore
--  NOT considered pinned. Local archives/descriptors and remote URLs are
--  already fixed and are therefore NOT flagged. Note: pecl has no git/VCS
--  install form, so no such case exists to handle.
dl3068 :: Rule ParsedShell
dl3068 = simpleRule code severity message check
  where
    code = "DL3068"
    severity = DLWarningC
    message =
      "Pin versions in pecl. Instead of `pecl install <package>` use `pecl install \
      \<package>-<version>`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotToPinVersion) args
    check _ = True
{-# INLINEABLE dl3068 #-}


forgotToPinVersion :: Shell.Command -> Bool
forgotToPinVersion cmd =
  isPeclInstall cmd && installIsFirst cmd && not (all versionFixed (packages cmd))

isPeclInstall :: Shell.Command -> Bool
isPeclInstall = Shell.cmdHasArgs "pecl" ["install"]

installIsFirst :: Shell.Command -> Bool
installIsFirst cmd = ["install"] `isPrefixOf` Shell.getArgsNoFlags cmd

packages :: Shell.Command -> [Text.Text]
packages cmd = stripInstallPrefix (Shell.getArgsNoFlags cmd)

versionFixed :: Text.Text -> Bool
versionFixed package
  | isLocalArtifact package = True
  | isRemoteUrl package = True
  | otherwise = hasNumericVersion (dropChannel package)

-- | A package is pinned only when the segment after the name/version
-- separator (`-`) starts with a digit. This rejects floating stability
-- keywords such as `redis-beta` while accepting `redis-6.1.0`.
hasNumericVersion :: Text.Text -> Bool
hasNumericVersion package =
  case Text.breakOnEnd "-" package of
    (prefix, versionPart)
      | Text.null prefix -> False -- no `-` at all, e.g. `redis`
      | otherwise -> maybe False (isDigit . fst) (Text.uncons versionPart)
  where
    isDigit c = c >= '0' && c <= '9'

-- | Drop an optional channel prefix so only the `<name>-<version>` segment
-- is inspected. Handles the `channel://` URI form, the `channel/name`
-- form, and the alias-free case.
dropChannel :: Text.Text -> Text.Text
dropChannel pkg =
  case Text.breakOnEnd "/" (stripScheme pkg) of
    (prefix, rest)
      | Text.null prefix -> pkg -- no `/`, nothing to strip
      | otherwise -> rest
  where
    stripScheme p = maybe p snd (uncons2 (Text.breakOn "://" p))
    -- If a `://` scheme is present, keep only what follows it.
    uncons2 (before, after)
      | Text.null after = Nothing
      | otherwise = Just (before, Text.drop (Text.length "://") after)

-- | Only genuine file artifacts and plain HTTP(S) downloads are already
-- fixed. This covers the documented local forms accepted by `pecl install`:
-- a package release archive (e.g. `PackageName-1.2.3.tgz`) and a
-- `package.xml` descriptor path, given as either an absolute or a relative
-- path. A `channel://` URI is NOT exempt here: it still carries a
-- `name-version` tail that must be pinned (handled via `dropChannel`).
isLocalArtifact :: Text.Text -> Bool
isLocalArtifact pkg =
  any (`Text.isSuffixOf` pkg) [".tgz", ".tar", ".tar.gz", ".tar.bz2", ".zip", ".xml"]

isRemoteUrl :: Text.Text -> Bool
isRemoteUrl pkg =
  any (`Text.isPrefixOf` pkg) ["http://", "https://"]

stripInstallPrefix :: [Text.Text] -> [Text.Text]
stripInstallPrefix cmd = dropWhile (== "install") (dropWhile (/= "install") cmd)
