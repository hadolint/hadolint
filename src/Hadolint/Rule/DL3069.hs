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
--    composer update  <vendor>/<package>:<constraint>
--    composer require <vendor>/<package> <constraint>   (version as a separate argument)
--  A package is considered pinned when it carries a non-empty version
--  constraint via `:` (e.g. `:^4.2`, `:~4.2`, `:4.5.0`, `:4.5@dev`,
--  `:dev-master#<hash>`) or when the version is supplied as the following
--  argument. The `:*` wildcard is NOT a pin and is therefore flagged, as is a
--  bare `<vendor>/<package>` with no constraint.
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

installIsFirst :: Shell.Command -> Bool
installIsFirst cmd = take 1 (Shell.getArgsNoFlags cmd) `elem` map pure subcommands

subcommands :: [Text.Text]
subcommands = ["require", "r", "update", "u", "upgrade"]

-- | The package arguments are the `<vendor>/<package>[:constraint]` tokens.
-- A trailing bare version argument (no `/`) is attached to the preceding
-- package so the `composer require vendor/pkg 4.5.0` form is recognised.
packages :: Shell.Command -> [Text.Text]
packages cmd = attachLooseVersions (stripSubcommand (Shell.getArgsNoFlags cmd))

versionFixed :: Text.Text -> Bool
versionFixed package =
  case Text.breakOn ":" package of
    (_, constraint)
      | Text.null constraint -> False -- no `:` and no attached version
      | otherwise -> isConcreteConstraint (Text.drop 1 constraint)

-- | A constraint is concrete unless it is empty or the `*` wildcard.
isConcreteConstraint :: Text.Text -> Bool
isConcreteConstraint constraint =
  not (Text.null constraint) && constraint /= "*"

-- | Attach a trailing version token (one without `/`) to the preceding
-- package token as a `:<version>` constraint, so `vendor/pkg 4.5.0` reads as
-- pinned. Tokens containing `/` are packages in their own right.
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
    hasConstraint t = ":" `Text.isInfixOf` t

stripSubcommand :: [Text.Text] -> [Text.Text]
stripSubcommand = dropWhile (not . isPackage)
  where
    isPackage t = "/" `Text.isInfixOf` t
