module Hadolint.Rule.DL3070 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax (Instruction (..), RunArgs (..))


rule :: Rule ParsedShell
rule = dl3070 <> onbuild dl3070
{-# INLINEABLE rule #-}

-- | Rule for pinning PIE (PHP Installer for Extensions) packages to a version.
--  PIE replaces PECL and resolves package/version arguments like Composer.
--  The `install`, `build` and `download` subcommands share the same argument
--  grammar: `{vendor/package}{?:{?version-constraint}{?@stability}}`, e.g.
--  `xdebug/xdebug:^3.4`, `xdebug/xdebug:^3.4@alpha`, `xdebug/xdebug:@alpha`,
--  `xdebug/xdebug:dev-main#<hash>`.
--  A package is considered pinned when it carries a non-empty constraint via
--  `:`; a bare `<vendor>/<package>` with no constraint is flagged. Invoking a
--  subcommand with no package argument (e.g. `pie install` for a project) is
--  not flagged.
dl3070 :: Rule ParsedShell
dl3070 = simpleRule code severity message check
  where
    code = "DL3070"
    severity = DLWarningC
    message =
      "Pin versions in pie. Instead of `pie install <vendor>/<package>` use \
      \`pie install <vendor>/<package>:<version>`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotToPinVersion) args
    check _ = True
{-# INLINEABLE dl3070 #-}


forgotToPinVersion :: Shell.Command -> Bool
forgotToPinVersion cmd =
  isPieInstall cmd && not (all versionFixed (packages cmd))

isPieInstall :: Shell.Command -> Bool
isPieInstall cmd =
  Shell.cmdHasArgs "pie" subcommands cmd && installIsFirst cmd

installIsFirst :: Shell.Command -> Bool
installIsFirst cmd =
  take 1 (Shell.getArgsNoFlags (Shell.dropFlagArg ignoreFlags cmd)) `elem` map pure subcommands

subcommands :: [Text.Text]
subcommands = ["install", "build", "download"]

-- | The package arguments are the `<vendor>/<package>[:constraint]` tokens
-- following the subcommand, with value-taking flags removed first.
packages :: Shell.Command -> [Text.Text]
packages cmd =
  stripSubcommand (Shell.getArgsNoFlags (Shell.dropFlagArg ignoreFlags cmd))

versionFixed :: Text.Text -> Bool
versionFixed package =
  case Text.breakOn ":" package of
    (_, constraint) -> not (Text.null (Text.drop 1 constraint))

stripSubcommand :: [Text.Text] -> [Text.Text]
stripSubcommand = filter (`notElem` subcommands)

-- | Flags that take a separate value argument which MUST NOT be mistaken for a
-- package specification. In particular `--select ext=vendor/package` and the
-- `--with-php-config=...` style options carry values that may contain a `/`.
ignoreFlags :: [Text.Text]
ignoreFlags =
  [ "with-php-config",
    "with-php-path",
    "with-phpize-path",
    "with-libdir",
    "select",
    "make-parallel-jobs",
    "j",
    "working-dir",
    "d",
    "suppress-download-url-method"
  ]
