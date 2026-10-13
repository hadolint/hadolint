module Hadolint.Rule.DL3069 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax (Instruction (..), RunArgs (..))


rule :: Rule ParsedShell
rule = dl3069 <> onbuild dl3069
{-# INLINEABLE rule #-}

-- | Rule for pinning Composer packages to a version.
--  Supported package specifications (per the Composer `require`/`update` CLI):
--    composer require <vendor>/<package>:<constraint>
--    composer require <vendor>/<package>=<constraint>
--    composer require "<vendor>/<package> <constraint>"  (single quoted argument)
--    composer require <vendor>/<package> <constraint>    (version as a separate argument)
--    composer global require <vendor>/<package>:<constraint>
--  Composer accepts `:`, `=` or a space as the name/version separator. A
--  package is considered pinned when it carries a non-empty version
--  constraint (e.g. `:^4.2`, `=4.5.0`, `:4.5@dev`, `:dev-master#<hash>`) or
--  when the version is supplied as the following argument. The `:*` wildcard
--  is NOT a pin and is therefore flagged, as is a bare `<vendor>/<package>`
--  with no constraint.
dl3069 :: Rule ParsedShell
dl3069 = simpleRule code severity message check
  where
    code = "DL3069"
    severity = DLWarningC
    message =
      "Pin versions in composer. Instead of `composer require <vendor>/<package>` use \
      \`composer require <vendor>/<package>:<version>`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotToPinVersion) args
    check _ = True
{-# INLINEABLE dl3069 #-}


forgotToPinVersion :: Shell.Command -> Bool
forgotToPinVersion cmd =
  isComposerInstall cmd && not (all versionFixed (packages cmd))

isComposerInstall :: Shell.Command -> Bool
isComposerInstall cmd =
  Shell.cmdHasArgs "composer" subcommands cmd && installIsFirst cmd

-- | The subcommand MUST be the first non-flag argument, optionally preceded by
-- `global` (as in `composer global require vendor/pkg`).
installIsFirst :: Shell.Command -> Bool
installIsFirst cmd =
  case dropWhile (== "global") (Shell.getArgsNoFlags (Shell.dropFlagArg ignoreFlags cmd)) of
    (sub : _) -> sub `elem` subcommands
    [] -> False

subcommands :: [Text.Text]
subcommands = ["require", "r", "update", "u", "upgrade"]

-- | The package arguments are the `<vendor>/<package>[<sep><constraint>]`
-- tokens following the subcommand, with value-taking flags removed first. A
-- trailing bare version argument (no `/`) is attached to the preceding package
-- so the `composer require vendor/pkg 4.5.0` form is recognised.
packages :: Shell.Command -> [Text.Text]
packages cmd =
  attachLooseVersions (Shell.getArgsNoFlags (Shell.dropFlagArg ignoreFlags cmd))

versionFixed :: Text.Text -> Bool
versionFixed package =
  case Text.break isSeparator package of
    (_, rest)
      | Text.null rest -> False -- no separator, hence no constraint
      | otherwise -> isConcreteConstraint (Text.drop 1 rest)

-- | Composer splits the package name from its version constraint on any of
-- these characters.
isSeparator :: Char -> Bool
isSeparator c = c == ':' || c == '=' || c == ' '

-- | A constraint is concrete unless it is empty or the `*` wildcard.
isConcreteConstraint :: Text.Text -> Bool
isConcreteConstraint constraint =
  not (Text.null constraint) && constraint /= "*"

-- | Attach a trailing version token (one without `/`) to the preceding
-- package token as a `:<version>` constraint, so `vendor/pkg 4.5.0` reads as
-- pinned. Tokens containing `/` are packages in their own right; any other
-- token (the subcommand, `global`, a stray version already consumed) is
-- skipped.
attachLooseVersions :: [Text.Text] -> [Text.Text]
attachLooseVersions [] = []
attachLooseVersions (p : rest)
  | isPackage p = case rest of
      (v : more)
        | not (isPackage v) && not (hasConstraint p) ->
            (p <> ":" <> v) : attachLooseVersions more
      _ -> p : attachLooseVersions rest
  | otherwise = attachLooseVersions rest
  where
    isPackage t = "/" `Text.isInfixOf` t
    hasConstraint = Text.any isSeparator

-- | Value-taking flags whose values MUST NOT be mistaken for a package. In
-- particular `-d`/`--working-dir` take a path that may contain a `/`.
ignoreFlags :: [Text.Text]
ignoreFlags = ["d", "working-dir"]
