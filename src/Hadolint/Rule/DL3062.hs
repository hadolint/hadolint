module Hadolint.Rule.DL3062 (rule) where

import Data.List (isPrefixOf)
import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax (Instruction (..), RunArgs (..))


rule :: Rule ParsedShell
rule = dl3062 <> onbuild dl3062
{-# INLINEABLE rule #-}

-- | Rule for pinning PECL packages to version
--  supported formats by Hadolint
--    pecl install [<@channel>/]<name>-<version>
dl3062 :: Rule ParsedShell
dl3062 = simpleRule code severity message check
  where
    code = "dl3062"
    severity = DLWarningC
    message =
      "Pin versions in pecl. Instead of `pecl install <package>` use `pecl install \
      \<package>-<version>`"

    check (Run (RunArgs args _)) = foldArguments (Shell.noCommands forgotToPinVersion) args
    check _ = True
{-# INLINEABLE dl3062 #-}


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
  | otherwise = hasVersionSymbol package

hasVersionSymbol :: Text.Text -> Bool
hasVersionSymbol package = "-" `Text.isInfixOf` dropChannel package
  where
    dropChannel pkg =
      if "/" `Text.isInfixOf` pkg
        then Text.dropWhile ('/' <) pkg
        else pkg

stripInstallPrefix :: [Text.Text] -> [Text.Text]
stripInstallPrefix cmd = dropWhile (== "install") (dropWhile (/= "install") cmd)
