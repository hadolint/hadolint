module Hadolint.Rule.DL3068 (rule) where

import Data.Char (isDigit)
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
--  A pinned version MUST be numeric (e.g. `redis-6.1.0`) or a shell variable
--  (e.g. `redis-$VER` / `redis-${VER}`). Stability keywords such as `-alpha`,
--  `-beta` and `-stable` float and are therefore NOT considered pinned. Local
--  archives/descriptors and remote URLs are already fixed and are therefore
--  NOT flagged. The value passed to `-D`/`--define` is ignored. Note: pecl has
--  no git/VCS install form, so no such case exists to handle.
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
installIsFirst cmd =
  ["install"] `isPrefixOf` Shell.getArgsNoFlags (Shell.dropFlagArg ignoreFlags cmd)

packages :: Shell.Command -> [Text.Text]
packages cmd =
  stripInstallPrefix (Shell.getArgsNoFlags (Shell.dropFlagArg ignoreFlags cmd))

versionFixed :: Text.Text -> Bool
versionFixed package
  | isLocalArtifact package = True
  | isRemoteUrl package = True
  | otherwise = hasPinnedVersion (dropChannel package)

-- | A package is pinned when the segment after the name/version separator
-- (`-`) is a concrete version. This is the case when it starts with a digit
-- (e.g. `redis-6.1.0`) or when it is a shell variable such as `$VER` or
-- `${VER}` (parameterised versions are considered pinned, matching DL3013
-- and DL3016). Floating stability keywords such as `redis-beta` are rejected.
hasPinnedVersion :: Text.Text -> Bool
hasPinnedVersion package =
  case Text.breakOnEnd "-" package of
    (prefix, versionPart)
      | Text.null prefix -> False -- no `-` at all, e.g. `redis`
      | otherwise -> maybe False isVersionStart (Text.uncons versionPart)
  where
    isVersionStart (c, _) = isDigit c || c == '$'

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

-- | Flags that take a separate value argument which MUST NOT be mistaken for a
-- package specification. `pecl install -D 'with-foo=yes' <package>` passes
-- build configuration via `-D`/`--define`; the value is dropped before the
-- package list is inspected.
ignoreFlags :: [Text.Text]
ignoreFlags = ["D", "define"]
